import tkinter as tk
from tkinter import messagebox
from src.logic import Logic

NUM_ROWS = 8
NUM_COLS = 4
CELL_SIZE = 90
LIGHT_SQ = "#EEEED2"
DARK_SQ = "#769656"
CANAL_COLOR = "#4169E1"

PIECE_COLORS = {
    "q": "#5B3CAF",
    "d": "#2AA198",
    "p": "#DC742A"
}

PIECE_SCALE = {
    "q": 0.78,
    "d": 0.60,
    "p": 0.42,
}

class Board(tk.Tk):
    def __init__(self, logic: Logic):
        super().__init__()
        self.title("Martian Chess")
        self.logic = logic
        
        total_w = NUM_COLS * CELL_SIZE
        total_h = NUM_ROWS * CELL_SIZE

        self.overrideredirect(False)
        self.configure(bg="#222")
        
        self.canvas = tk.Canvas(self, width=total_w, height=total_h, bg="#222", 
                               highlightthickness=0, relief="flat", bd=0, 
                               highlightbackground="#222", highlightcolor="#222")
        self.canvas.pack(side=tk.TOP, fill=tk.BOTH, expand=False, padx=0, pady=0)

        self.drag_state = None

        self.canvas.tag_bind("piece", "<Button-1>", self.on_piece_press)
        self.canvas.bind("<B1-Motion>", self.on_piece_motion)
        self.canvas.bind("<ButtonRelease-1>", self.on_piece_release)

        self.render_position()

    def board_to_canvas_coords(self, row_index: int, col_index: int):
        x0 = col_index * CELL_SIZE
        y0 = row_index * CELL_SIZE
        x1 = x0 + CELL_SIZE
        y1 = y0 + CELL_SIZE
        return x0, y0, x1, y1

    def draw_grid(self):
        for r in range(NUM_ROWS):
            for c in range(NUM_COLS):
                x0, y0, x1, y1 = self.board_to_canvas_coords(r, c)
                is_dark = (r + c) % 2 == 1
                fill = DARK_SQ if is_dark else LIGHT_SQ
                self.canvas.create_rectangle(x0, y0, x1, y1, fill=fill, outline=fill)
        
        canal_y = 4 * CELL_SIZE
        self.canvas.create_line(0, canal_y, NUM_COLS * CELL_SIZE, canal_y, 
                               fill=CANAL_COLOR, width=4)

        file_labels = ["a", "b", "c", "d"]
        for c, label in enumerate(file_labels):
            x0, y0, x1, y1 = self.board_to_canvas_coords(NUM_ROWS - 1, c)
            is_dark = ((NUM_ROWS - 1) + c) % 2 == 1
            text_color = LIGHT_SQ if is_dark else DARK_SQ
            self.canvas.create_text(x1 - 9, y1 - 9, text=label, 
                                  font=("TkDefaultFont", 10, "bold"), fill=text_color)

        for r in range(NUM_ROWS):
            rank_num = str(NUM_ROWS - 1 - r)
            x0, y0, x1, y1 = self.board_to_canvas_coords(r, 0)
            is_dark = (r + 0) % 2 == 1
            text_color = LIGHT_SQ if is_dark else DARK_SQ
            self.canvas.create_text(x0 + 6, y0 + 10, text=rank_num, 
                                  font=("TkDefaultFont", 10, "bold"), fill=text_color)

    def draw_pyramid(self, row_index: int, col_index: int, kind: str):
        x0, y0, x1, y1 = self.board_to_canvas_coords(row_index, col_index)
        cx = (x0 + x1) / 2
        cy = (y0 + y1) / 2

        scale = PIECE_SCALE[kind]
        height = CELL_SIZE * scale
        base = height * 0.9
        color = PIECE_COLORS[kind]

        top = (cx, cy - height / 2)
        left = (cx - base / 2, cy + height / 2)
        right = (cx + base / 2, cy + height / 2)

        return self.canvas.create_polygon(
            [top[0], top[1], left[0], left[1], right[0], right[1]],
            fill=color,
            outline="black",
            width=2,
            smooth=False,
            joinstyle="miter",
        )

    def render_position(self):
        self.canvas.delete("all")
        self.draw_grid()
        for r in range(NUM_ROWS):
            for c in range(NUM_COLS):
                piece = self.logic.ranks[r][c]
                if piece in ("q", "d", "p"):
                    item_id = self.draw_pyramid(r, c, piece)
                    self.canvas.itemconfig(item_id, tags=(
                        "piece",
                        f"rc_{r}_{c}",
                        f"kind_{piece}",
                    ))
        print(f"\n=== GAME STATE ===")
        print(f"Current Player: {self.logic.current_player}")
        print(f"Current FEN: {self.logic.ranks_to_fen()}")
        print(f"Score: Top = {self.logic.score_top}, Bottom = {self.logic.score_bottom}")
        print(f"Last Move: {self.logic.last_move if self.logic.last_move else 'None'}")
        print("=" * 18)

    def on_piece_press(self, event):
        item = event.widget.find_withtag("current")
        if not item:
            return
        item_id = item[0]
        tags = self.canvas.gettags(item_id)
        rc_tag = next((t for t in tags if t.startswith("rc_")), None)
        kind_tag = next((t for t in tags if t.startswith("kind_")), None)
        if rc_tag is None or kind_tag is None:
            return
        _, r_str, c_str = rc_tag.split("_")
        kind = kind_tag.split("_", 1)[1]
        self.canvas.tag_raise(item_id)
        self.drag_state = {
            "item_id": item_id,
            "from_r": int(r_str),
            "from_c": int(c_str),
            "kind": kind,
            "last_x": event.x,
            "last_y": event.y,
            "allowed_to": set(),
        }

        self.clear_hints()
        from_alg = self.rc_to_alg(int(r_str), int(c_str))
        
        piece_names = {"q": "Queen", "d": "Drone", "p": "Pawn"}
        print(f"\n--- PIECE SELECTED ---")
        print(f"Piece: {piece_names.get(kind, kind)} at {from_alg}")
        print(f"Player: {self.logic.current_player}")
        
        moves = self.logic.get_moves(from_alg, kind)
        allowed_to = set()
        for m in moves:
            if "-" in m:
                _, to_alg = m.split("-", 1)
                allowed_to.add(to_alg.strip())
        
        # Print available moves
        print(f"Available moves: {list(allowed_to) if allowed_to else 'None'}")
        print("-" * 22)
        
        self.drag_state["allowed_to"] = allowed_to
        if allowed_to:
            self.draw_move_hints(allowed_to)

    def on_piece_motion(self, event):
        if not self.drag_state:
            return
        dx = event.x - self.drag_state["last_x"]
        dy = event.y - self.drag_state["last_y"]
        self.canvas.move(self.drag_state["item_id"], dx, dy)
        self.drag_state["last_x"] = event.x
        self.drag_state["last_y"] = event.y

    def on_piece_release(self, event):
        if not self.drag_state:
            return
        col = int(event.x // CELL_SIZE)
        row = int(event.y // CELL_SIZE)
        if col < 0:
            col = 0
        if col >= NUM_COLS:
            col = NUM_COLS - 1
        if row < 0:
            row = 0
        if row >= NUM_ROWS:
            row = NUM_ROWS - 1

        from_r = self.drag_state["from_r"]
        from_c = self.drag_state["from_c"]

        target_alg = self.rc_to_alg(row, col)
        self.clear_hints()
        if (row, col) != (from_r, from_c) and target_alg in (self.drag_state.get("allowed_to") or set()):
            move_str = f"{self.rc_to_alg(from_r, from_c)}-{target_alg}"
            
            print(f"\n*** MOVE EXECUTED ***")
            print(f"Move: {move_str}")
            print(f"Player: {self.logic.current_player}")
            
            if self.logic.make_move(move_str):
                print(f"Move successful!")
                print(f"New Score - Top: {self.logic.score_top}, Bottom: {self.logic.score_bottom}")
                self.render_position()
                winner = self.logic.check_winner()
                if winner:
                    print(f"\nGAME OVER! {winner} wins!")
                    messagebox.showinfo("Game Over", f"{winner} wins!")
                    self.canvas.tag_unbind("piece", "<Button-1>")
                    self.canvas.unbind("<B1-Motion>")
                    self.canvas.unbind("<ButtonRelease-1>")
                else:
                    self.logic.switch_player()
                    print(f"Next turn: {self.logic.current_player}")
                print("***" + "*" * 15)
            else:
                print("Move failed!")
        else:
            print(f"\n--- MOVE CANCELLED ---")
            print(f"Target {target_alg} is not a valid move")
            print("-" * 22)

        self.drag_state = None
        self.render_position()

    def rc_to_alg(self, r: int, c: int) -> str:
        files = ["a", "b", "c", "d"]
        rank_num = str(NUM_ROWS - 1 - r)
        return f"{files[c]}{rank_num}"

    def alg_to_rc(self, alg: str):
        files = {"a": 0, "b": 1, "c": 2, "d": 3}
        if not alg:
            return None
        file_char = alg[0].lower()
        file_idx = files.get(file_char)
        try:
            rank = int(alg[1:])
        except ValueError:
            return None
        if file_idx is None or rank < 0 or rank >= NUM_ROWS:
            return None
        row = NUM_ROWS - 1 - rank
        col = file_idx
        return row, col

    def clear_hints(self):
        self.canvas.delete("hint")

    def draw_move_hints(self, to_algs: set):
        for alg in to_algs:
            rc = self.alg_to_rc(alg)
            if not rc:
                continue
            r, c = rc
            x0, y0, x1, y1 = self.board_to_canvas_coords(r, c)
            cx = (x0 + x1) / 2
            cy = (y0 + y1) / 2
            radius = CELL_SIZE * 0.18
            self.canvas.create_oval(
                cx - radius,
                cy - radius,
                cx + radius,
                cy + radius,
                fill="#FFD54F",
                outline="#8D6E63",
                width=2,
                tags=("hint",),
            )
