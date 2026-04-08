from src.logic import Logic
from src.board import Board

BINARY_PATH = "./bin/martian-chess"

if __name__ == "__main__":
    logic = Logic(BINARY_PATH)
    Board(logic).mainloop()
