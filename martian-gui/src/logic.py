from src.haskell_interface import HaskellInterface

START_FEN = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"

NUM_ROWS = 8
NUM_COLS = 4

class Logic:
    def __init__(self, binary_path : str):
        self.engine = HaskellInterface(binary_path)
        self.current_player = "Bottom"
        self.score_top = 0
        self.score_bottom = 0
        self.last_move = ""
        self.ranks = self.parse_fen_like(START_FEN)

    def parse_fen_like(self, fen: str):
        fen = fen.strip()
        if not fen:
            raise ValueError("Empty FEN string")
        raw_ranks = fen.split("/")
        if len(raw_ranks) > NUM_ROWS:
            raise ValueError(f"Too many ranks: got {len(raw_ranks)}, expected at most {NUM_ROWS}")
        ranks = []
        for raw in raw_ranks:
            rank_cells = []
            segment = raw.strip().lower()

            if segment == "":
                rank_cells = [None] * NUM_COLS
            else:
                for ch in segment:
                    if ch in ("q", "d", "p"):
                        rank_cells.append(ch)
                    elif ch.isdigit():
                        n = int(ch)
                        if n < 1 or n > NUM_COLS:
                            raise ValueError(f"Digit out of range in rank '{raw}': {ch}")
                        rank_cells.extend([None] * n)
                    else:
                        raise ValueError(f"Invalid character in rank '{raw}': {ch}")
            if len(rank_cells) != NUM_COLS:
                raise ValueError(f"Rank '{raw}' expands to {len(rank_cells)} cells, expected {NUM_COLS}")
            ranks.append(rank_cells)
        while len(ranks) < NUM_ROWS:
            ranks.append([None] * NUM_COLS)
        return ranks

    def get_moves(self, from_alg: str, piece_kind: str):
        try:
            board_fen = self.ranks_to_fen()
            if piece_kind == "p":
                return self.engine.pawn_moves(board_fen, self.current_player, from_alg, self.last_move)
            elif piece_kind == "d":
                return self.engine.drone_moves(board_fen, self.current_player, from_alg, self.last_move)
            else:
                return self.engine.queen_moves(board_fen, self.current_player, from_alg, self.last_move)
        except Exception:
            return []

    def make_move(self, move_str: str):
        try:
            board_fen = self.ranks_to_fen()
            new_fen, gained = self.engine.make_move(board_fen, move_str)
            if new_fen:
                self.ranks = self.parse_fen_like(new_fen)
                if self.current_player == "Top":
                    self.score_top += int(gained)
                else:
                    self.score_bottom += int(gained)
                self.last_move = move_str
                return True
        except Exception:
            pass
        return False

    def check_winner(self):
        try:
            return self.engine.player_won(self.ranks_to_fen(), self.current_player, self.score_top, self.score_bottom)
        except Exception:
            return None

    def switch_player(self):
        self.current_player = "Bottom" if self.current_player == "Top" else "Top"

    def ranks_to_fen(self) -> str:
        parts = []
        for r in range(NUM_ROWS):
            empties = 0
            row_parts = []
            for c in range(NUM_COLS):
                piece = self.ranks[r][c]
                if piece in ("q", "d", "p"):
                    if empties:
                        row_parts.append(str(empties))
                        empties = 0
                    row_parts.append(piece)
                else:
                    empties += 1
            if empties:
                row_parts.append(str(empties))
            parts.append("".join(row_parts) if row_parts else str(NUM_COLS))
        parts = ["" if p == "4" else p for p in parts]
        return "/".join(parts)