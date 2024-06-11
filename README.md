# ps2-to-plus-keyboard

An adapter to connect a PS/2 keyboard to a Macintosh Plus/512k/128k.


## Technical Details

### Key Mapping

Key mapping is defined by a lookup table in the firmware and is relatively easily changed.  Defaults have been chosen to be as faithful to the genuine Plus keyboard as possible.

Non-obvious mappings defined in the firmware lookup table as shipped are as follows:

| PS/2 Key     | Plus Key                      | User Programmable? |
| ------------ | ----------------------------- | ------------------ |
| Alt          | Option                        | No                 |
| Ctrl         | Command                       | No                 |
| Super        | Command                       | No                 |
| Apps         | Enter (key right of spacebar) | No                 |
| Insert       | No Mapping                    | No                 |
| Delete       | No Mapping                    | No                 |
| Home         | No Mapping                    | No                 |
| End          | No Mapping                    | No                 |
| Page Up      | No Mapping                    | No                 |
| Page Down    | No Mapping                    | No                 |
| Num Lock     | Clear                         | No                 |
| Print Screen | Command-Shift-3               | No                 |
| Scroll Lock  | Command-Shift-2               | No                 |
| Pause        | Command-Shift-1               | No                 |
| Escape       | Command-.                     | Yes                |
| F1           | Command-Z                     | Yes                |
| F2           | Command-X                     | Yes                |
| F3           | Command-C                     | Yes                |
| F4           | Command-V                     | Yes                |
| F5           | No Mapping                    | Yes                |
| F6           | No Mapping                    | Yes                |
| F7           | No Mapping                    | Yes                |
| F8           | No Mapping                    | Yes                |
| F9           | No Mapping                    | Yes                |
| F10          | No Mapping                    | Yes                |
| F11          | No Mapping                    | Yes                |
| F12          | No Mapping                    | Yes                |
| ACPI Power   | No Mapping                    | Yes                |
| ACPI Sleep   | No Mapping                    | Yes                |
| ACPI Wake    | No Mapping                    | Yes                |


### User Programmable Keys

Keys marked as being user programmable in the table above can have their function changed on the fly.  This is done by holding down the key to be reprogrammed while typing the keystroke(s) that it should replay when pressed.  Programmed key functions are stored in high-endurance flash memory and persist when power is removed.

Programmable key functions consist of a sequence of events and may consume up to 16 bytes each.  Note that a key press and a key release are two separate events.  Most events consume one byte each, but keypad keys (including the arrow keys) require an extra byte and may also require the firmware to modify the state of the shift key, which consumes an extra two bytes.


### Building Firmware

Building the firmware requires Microchip MPASM, which is included with their development environment, MPLAB. Note that you must use MPLAB X version 5.35 or earlier or MPLAB 8 as later versions of MPLAB X have removed MPASM.
