# **Nand2Tetris From Project 7 (VM Translator)**

## Introduction

In this project, I will be working directly on Project 7 of the Nand2Tetris course, which involves writing a VM translator that converts high-level VM commands into Hack assembly language. To accomplish this, I have chosen to write the components of the translator in Pascal.

## Setup

### Required Downloads and Tools

**Free Pascal Compiler (FPC)**: This is the compiler I will use to compile my Pascal code. You can download it from [here](https://www.freepascal.org/download.html).

**Visual Studio Code (VSCode)**: This is the code editor I will use to write my Pascal code. Download it from [here](https://code.visualstudio.com/).

### VSCode Extensions

To enhance my coding experience, I have installed the following extensions in VSCode:

- **Pascal**: Provides support for Pascal language syntax highlighting and code snippets.
- **Code Runner**: Allows me to run Pascal programs directly within the VSCode environment.

### Setting Up FPC in Object Mode

By default, FPC is not in object mode, which is necessary for object-oriented programming. To enable object mode, add the following line at the top of each Pascal file:

```pascal
{$mode objfpc}
