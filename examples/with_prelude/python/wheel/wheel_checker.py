import importlib.resources
import zipfile


def main() -> None:
    with importlib.resources.path(__package__, "printlib.whl") as wheel:
        with zipfile.ZipFile(wheel) as zf:
            if "printlib/print.py" in zf.namelist():
                print("Wheel OK")
            else:
                print(f"print.py not found in {wheel=}. {zf.namelist()=}")


if __name__ == "__main__":
    main()
