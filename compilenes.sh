#!/bin/bash

ca65 nes_template.asm -o nes_template.o -t nes
ld65 nes_template.o -o nes_template.nes -t nes
