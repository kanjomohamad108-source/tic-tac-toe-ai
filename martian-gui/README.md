# Martian Chess GUI

A Python graphical user interface (GUI) for the game Martian Chess, implemented using Tkinter. This program allows players to interact with the game board and pieces through a visual interface.

## Usage

**Place the Executable**

   - Ensure you have the `martian-chess` executable file.
   - It can be found in the hidden directory `.stack-work/install/...` in your stack project.
   - Place it in the `bin` directory.
   - Or: Update the `EXECUTABLE_PATH` variable in `main.py` to point to the correct path.

```python
BINARY_PATH = "/path/to/your/binary/martian-chess"
```
     
**Run the GUI**

In your GUI folder, run:

```bash
python main.py
```

The game window will appear, and you can start interacting with the game board by dragging and dropping the pieces.

**Note:** This program serves only as a graphical user interface (GUI); your Haskell program is responsible for enforcing the game rules. The GUI simply executes your compiled Haskell binary, which returns the results from your functions as a string.

You can change the displayed board configuration by changing the FEN string in `src/logic.py`:

```python
START_FEN = "qqd1/qdp1/dpp1///1ppd/1pdq/1dqq"
```
