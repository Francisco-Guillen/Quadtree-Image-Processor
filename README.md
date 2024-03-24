# Quadtree Image Processor

This OCaml program implements a quadtree-based image processor. It constructs a quadtree representation of a black-and-white image and provides various operations such as calculating the number of leaves and nodes in the quadtree, finding the shortest and longest branches, and performing transformations like rotation and inversion of colors.

## Overview

The program takes as input a black-and-white image in PBM (Portable BitMap) format and processes it using quadtree data structures. It provides functionalities to analyze the quadtree, perform transformations, and visualize the results.

## Features

- **Quadtree Construction**: Constructs a quadtree representation of the input image.
- **Analysis**: Calculates the number of leaves and nodes in the quadtree, as well as the lengths of the shortest and longest branches.
- **Transformations**: Provides operations to rotate the image by 90 degrees, invert colors, and rotate the image by 180 degrees.
- **Output**: Displays the analysis results and the transformed images.

## Usage

To use the program, follow these steps:

1. Prepare a black-and-white image in PBM format.
2. Run the program and provide the image as input.
3. The program will process the image and display analysis results along with transformed images.

## Input Format

The input format follows the PBM format, which consists of a header line starting with "P1" followed by the width and height of the image in pixels. Subsequent lines represent the image pixels, where 0 represents white and 1 represents black.

## Bibliography

- [Netpbm Format - Wikipedia](https://en.wikipedia.org/wiki/Netpbm)
- [Quadtree - GeeksforGeeks](https://www.geeksforgeeks.org/quad-tree/)
- [OCaml Array Module Documentation](https://v2.ocaml.org/api/Array.html)

Feel free to explore and modify the code according to your needs!
