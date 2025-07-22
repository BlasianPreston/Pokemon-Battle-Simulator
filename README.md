# OCaml Pokémon Battle Simulator

## 🚀 Getting Started

This is a graphical Pokémon battle simulator written in OCaml for my CS3110 Functional Programming class using SDL2 bindings (`Tsdl` and `Tsdl_ttf`).

To build and run the project:

```sh
dune build
dune exec bin/main.exe
```

Make sure you have:
- Dune (>= 3.0)
- OCaml (>= 5.0)
- SDL2 installed on your system
- Required OCaml libraries: `tsdl`, `tsdl-ttf`

You can install these with OPAM:

```sh
opam install tsdl tsdl-ttf
```

---

## 🎮 Gameplay

The simulator provides a turn-based Pokémon battle system with a custom graphical interface. Players interact with the game using menu buttons rendered via SDL2.

### Features:
- **Graphical UI** powered by `Tsdl` and `Tsdl_ttf`
- **Sprite-based visuals** for Pokémon and backgrounds (`assets/sprites/`)
- **Font rendering** using `Helvetica.ttf` in `assets/fonts/`
- **Main menu navigation**
- **Turn-based battle logic** between player's and AI-controlled Pokémon
- Type effectiveness
- Move animations
- Item usage

---

### 🖼️ Screenshots & Diagrams

<!-- To add an image or gif of the gameplay, use the following Markdown syntax: -->

This is what will show up after you run the command to start the game

```md
![Team Choosing Screen]<img width="799" height="596" alt="team" src="https://github.com/user-attachments/assets/307edd01-5671-4459-a2f7-bd81a3b8b9f8" />
```

After you choose your team, this is what your game may look like

```md
![Battle Gameplay]<img width="801" height="598" alt="battle" src="https://github.com/user-attachments/assets/858ecc4d-cd9b-4f95-b45f-0c801ecb1a2f" />
```

---

## 🧱 Project Structure

```
lib/
├── battle.ml       - Core battle logic and turn resolution
├── move.ml         - Definitions and behavior of Pokémon moves
├── item.ml         - Items and their effects
├── pokemon.ml      - Pokémon data and status handling
├── pType.ml        - Pokémon type definitions and utilities
bin/
├── main.ml         - Main game loop and SDL-based UI
test/
├── test_final_project.ml - Test suite for battle and logic
assets/
├── fonts/
│   └── Helvetica.ttf
├── sprites/
│   └── (contains .bmp sprite images)
```

---


## 🚧 Future Improvements

- Improve AI with smarter decision-making
- Add support for status effects (burn, poison, etc.)
- More move animations and feedback
- Expand sprite and Pokémon database
- Multiplayer or networked battles
- Sound effects and background music
- Improved graphics

---

## 🧪 Running Tests

If you have tests in `test/test_final_project.ml`, you can run them with:

```sh
dune runtest
```

> Make sure `test/dune` exists and includes the test module in the `tests` stanza.

---

## 👥 Team Members

- Preston Williams
- Dicky Ning
- Sean Aidarhanov
- Pedro Arruda
- Samir Amin

---

