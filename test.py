import os
import glob

import os
import glob
import chardet
from tqdm import tqdm

def read_and_dump_files(directory):
    """Reads files with various encodings and dumps their contents into a text file."""

    with open("cd.txt", "w", encoding="utf-8") as outfile:
        file_paths = glob.glob(os.path.join(directory, "*.dbf"))
        # ... (add other extensions as needed)

        for file_path in tqdm(file_paths , total = len(file_paths)):
            outfile.write(f"--- File: {file_path} ---\n")
            with open(file_path, "rb") as infile:  # Open in binary mode
                result = chardet.detect(infile.read())  # Detect encoding
                encoding = result['encoding']
                if encoding:
                    try:
                        with open(file_path, "r", encoding=encoding) as infile:
                            outfile.write(infile.read())
                    except UnicodeDecodeError:
                        outfile.write("[Error: Unable to decode file content despite encoding detection]\n")
                else:
                    outfile.write("[Error: Unable to detect file encoding]\n")
            outfile.write("\n")
# Example usage:
directory_path = "/workspaces/codespaces-blank/hehe"  # Replace with the actual directory path
read_and_dump_files(directory_path)