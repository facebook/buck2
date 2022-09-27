import os

if __name__ == "__main__":
    original_main = os.getenv("PAR_MAIN_ORIGINAL")
    print(f"Hello from main wrapper for {original_main}!")
