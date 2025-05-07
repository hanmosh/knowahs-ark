#!/usr/bin/env python3
"""
run_clue.py - Python interface for Clue/Cluedo game visualization
"""

import os
import subprocess
import sys
import tempfile
from pyswip import Prolog
import webbrowser
import time

def ensure_graphviz_installed():
    """Check if GraphViz is installed"""
    try:
        subprocess.run(['dot', '-V'], capture_output=True, check=True)
        print("GraphViz is installed.")
        return True
    except (subprocess.CalledProcessError, FileNotFoundError):
        print("ERROR: GraphViz is not installed or not in PATH.")
        print("Please install GraphViz from https://graphviz.org/download/")
        return False

def run_visualization():
    """Run the Prolog visualization to generate a graph image"""
    # Initialize Prolog
    prolog = Prolog()

    # Load visualization logic
    print("Loading Prolog knowledge base and visualization logic...")
    prolog.consult("clue_kb.pl")
    prolog.consult("clue_viz.pl")

    # Load the sample game state
    print("Loading sample game state...")
    prolog.consult("sample_game_state.pl")

    # Generate the visualization
    output_file = "clue_game_state.png"
    print(f"Generating visualization to {output_file}...")

    # Create and execute a temporary DOT file with Python
    dot_file = tempfile.NamedTemporaryFile(suffix='.dot', delete=False)
    dot_file_path = dot_file.name

    # Get DOT content from Prolog
    dot_content = None
    for result in prolog.query("generate_game_graph(DotStr)"):
        dot_content = result["DotStr"]
        break

    if not dot_content:
        print("ERROR: Failed to generate graph from Prolog.")
        return False

    # Write DOT content to file
    with open(dot_file_path, 'w') as f:
        f.write(dot_content)

    # Run GraphViz to generate PNG
    try:
        subprocess.run(['dot', '-Tpng', dot_file_path, '-o', output_file], check=True)
        print(f"Visualization generated successfully: {output_file}")

        # Open the generated image
        if os.path.exists(output_file):
            print(f"Opening {output_file}...")
            if sys.platform == 'darwin':  # macOS
                subprocess.run(['open', output_file])
            elif sys.platform == 'win32':  # Windows
                os.startfile(output_file)
            else:  # Linux
                subprocess.run(['xdg-open', output_file])
        return True
    except subprocess.CalledProcessError as e:
        print(f"ERROR running GraphViz: {e}")
        return False
    finally:
        # Clean up temporary file
        try:
            os.unlink(dot_file_path)
        except:
            pass

def direct_prolog_visualization():
    """Run visualization directly through Prolog"""
    try:
        output_file = "clue_game_state.png"
        result = subprocess.run(
            ['swipl', '-q', '-t', f"consult('clue_viz.pl'), consult('sample_game_state.pl'), visualize_game_state('{output_file}'), halt"],
            check=True,
            capture_output=True
        )
        print(f"Visualization generated successfully: {output_file}")

        # Open the generated image
        if os.path.exists(output_file):
            print(f"Opening {output_file}...")
            if sys.platform == 'darwin':  # macOS
                subprocess.run(['open', output_file])
            elif sys.platform == 'win32':  # Windows
                os.startfile(output_file)
            else:  # Linux
                subprocess.run(['xdg-open', output_file])
        return True
    except subprocess.CalledProcessError as e:
        print(f"ERROR running Prolog visualization: {e}")
        print(f"STDOUT: {e.stdout.decode('utf-8')}")
        print(f"STDERR: {e.stderr.decode('utf-8')}")
        return False

def main():
    print("Clue/Cluedo Game Knowledge Base Visualization")
    print("=============================================")

    if not ensure_graphviz_installed():
        return

    print("\nChoose visualization method:")
    print("1. Use PySwip (Python interface to SWI-Prolog)")
    print("2. Run SWI-Prolog directly")

    choice = input("\nEnter your choice (1/2): ").strip()

    if choice == '1':
        try:
            from pyswip import Prolog
            run_visualization()
        except ImportError:
            print("ERROR: PySwip is not installed.")
            print("Install it with: pip install pyswip")
            print("Falling back to direct SWI-Prolog...")
            direct_prolog_visualization()
    else:
        direct_prolog_visualization()

if __name__ == "__main__":
    main()
