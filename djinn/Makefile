# Define compiler
CC := gcc

# Include directories
INCLUDES := -I vendor/glad/include -I vendor/glfw-3.3.8/include -I ./

# Source files
SOURCES := vendor/glad/src/glad.c djinn.c render.c bin/djinn_example_app.c

# Output directory and binary
OUTDIR := build
OUTPUT := $(OUTDIR)/djinn_example_app

# Libraries
LIBS := -lm -lglfw

# Compiler flags
CFLAGS := $(INCLUDES) $(LIBS)

.PHONY: all clean prepare

all: prepare $(OUTPUT)

prepare:
	@mkdir -p $(OUTDIR)

$(OUTPUT): $(SOURCES)
	$(CC) $(SOURCES) $(CFLAGS) -o $(OUTPUT) && ./$@

clean:
	rm -rf $(OUTDIR)
