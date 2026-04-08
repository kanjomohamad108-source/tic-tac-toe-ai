import subprocess
from typing import List, Optional, Tuple
import os

class HaskellInterface:

    def __init__(self, binary_path: str):
        self.binary_path = binary_path
        if not os.path.isfile(self.binary_path):
            raise FileNotFoundError(f"Binary not found: {self.binary_path}")
        if not os.access(self.binary_path, os.X_OK):
            raise PermissionError(f"Binary is not executable: {self.binary_path}")

    def _run(self, function_name: str, *args: str) -> str:
        cmd = [self.binary_path, function_name, *[str(a) for a in args]]
        out = subprocess.check_output(cmd, text=True)
        return out.strip()

    def parse_moves_list(self, output: str) -> List[str]:
        s = (output or "").strip()
        if not s:
            return []
        if s.startswith("[") and s.endswith("]"):
            s = s[1:-1]
        if not s:
            return []
        parts = [p.strip() for p in s.split(",")]
        return [p[1:-1] if (len(p) >= 2 and ((p[0] == '"' and p[-1] == '"') or (p[0] == "'" and p[-1] == "'"))) else p for p in parts if p]

    def parse_bool(self, output: str) -> bool:
        s = (output or "").strip().lower()
        return s == "true"

    def parse_int(self, output: str) -> int:
        s = (output or "").strip()
        if not s:
            return 0
        return int(s)

    def pawn_moves(self, board: str, player: str, pos: str, last_move: str) -> List[str]:
        return self.parse_moves_list(self._run("pawnMoves", board, player, pos, last_move))

    def drone_moves(self, board: str, player: str, pos: str, last_move: str) -> List[str]:
        return self.parse_moves_list(self._run("droneMoves", board, player, pos, last_move))

    def queen_moves(self, board: str, player: str, pos: str, last_move: str) -> List[str]:
        return self.parse_moves_list(self._run("queenMoves", board, player, pos, last_move))

    def make_move(self, board: str, move: str) -> Tuple[str, int]:
        out = self._run("makeMove", board, move)
        s = (out or "").strip()
        if s.startswith("(") and s.endswith(")"):
            s = s[1:-1]
        if "," in s:
            fen_part, score_part = s.rsplit(",", 1)
            return fen_part.strip(), self.parse_int(score_part)
        parts = s.split()
        if len(parts) >= 2:
            return " ".join(parts[:-1]).strip(), self.parse_int(parts[-1])
        return s, 0

    def player_won(self, board: str, player: str, scores_top: int, scores_bottom: int) -> Optional[str]:
        out = self._run("playerWon", board, player, scores_top, scores_bottom)
        s = (out or "").strip()
        if s == "Nothing":
            return None
        return s
