#!/usr/bin/env python3
"""Reconstruct the 19 columns of the supplied Honduras school PDF.

Install at code/python/school_pdf_to_csv.py. This is a new reconstruction,
not a recovered copy of the earlier ChatGPT/PyMuPDF script.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import json
import os
from pathlib import Path
import platform
import re
import sys
import tempfile
from collections import Counter
from importlib.metadata import version

try:
    import pymupdf
except ImportError:
    sys.exit("Missing dependency. Install school_pdf_requirements.txt with this Python interpreter.")

SOURCE_SHA256 = "3560d54ec981f952a6317e180304790642d6641a0c78e49cd1bd133bef26b09f"
EXPECTED_PAGES = 71
EXPECTED_ROWS = 22675
FIELDS = (
    "departamento", "municipio", "aldea", "zona_escolar", "codigo_centro",
    "nombre_centro", "nivel_educativo", "direccion_centro", "nombre_director",
    "ninas", "ninos", "total", "mobiliario", "servicios_basicos",
    "amenazas_naturales", "amenazas_sociales", "hidrosanitaria",
    "infraestructura", "indice_precariedad",
)
# PDF points, measured from the supplied original. The first nine columns are
# left aligned; the ten numerical columns are right aligned. Using each PDF
# text-show operation separately preserves text that overflows a printed cell.
TEXT_STARTS = (51.6, 72.12, 95.184, 113.9, 131.54, 150.5, 191.3, 232.13, 292.61)
NUMBER_ENDS = (354.797, 371.597, 388.517, 406.756, 428.866,
               449.506, 467.506, 492.106, 515.866, 532.066)
TOLERANCE = 0.08


class ExtractionError(ValueError):
    """The input does not satisfy the documented extraction assumptions."""


def text_entries(page):
    """Read raw drawing spans; avoid TextPage's merging of overlapping cells.

    In this specific PDF each trace span represents a single table entry.
    Verify that assumption instead of silently accepting a different layout.
    PyMuPDF coordinates run from the top of the page downward.
    """
    runs = []
    for span in page.get_texttrace():
        chars = span["chars"]
        if not chars:
            continue
        if span["dir"] != (1.0, 0.0) or span["wmode"] != 0:
            raise ExtractionError("Unexpected rotated/vertical text.")
        if len({round(c[2][1], 3) for c in chars}) != 1:
            raise ExtractionError("A text entry spans multiple rows.")
        text = "".join(chr(c[0]) for c in chars)
        if "\ufffd" in text:
            raise ExtractionError("An undecodable character was found in the PDF.")
        runs.append((text, chars[0][2][0], chars[0][2][1], chars[-1][3][2]))
    return runs


def column_for(text, x0, x1):
    for index, start in enumerate(TEXT_STARTS):
        if abs(x0 - start) <= TOLERANCE:
            return index
    for index, end in enumerate(NUMBER_ENDS, start=9):
        if abs(x1 - end) <= TOLERANCE and re.fullmatch(r"[0-9,]+(?:\.[0-9]+)?", text):
            return index
    raise ExtractionError(f"Unrecognised text entry at x={x0:.3f}..{x1:.3f}: {text!r}")


def extract_rows(source):
    rows, counts, locations = [], [], []
    with pymupdf.open(source) as document:
        for page_no, page in enumerate(document, 1):
            if page.rotation != 0:
                raise ExtractionError(f"Unexpected rotation on page {page_no}.")
            runs = text_entries(page)
            # Use every entry at the school-code origin as a row anchor,
            # including malformed codes, which validation will then reject.
            baselines = [round(y, 3) for text, x, y, end in runs
                         if text.strip() and abs(x - TEXT_STARTS[4]) <= TOLERANCE]
            if not baselines or len(baselines) != len(set(baselines)):
                raise ExtractionError(f"Missing or duplicated row anchors on page {page_no}.")
            cells = {y: [None] * len(FIELDS) for y in baselines}
            for text, x, y, end in runs:
                text = " ".join(text.split())
                if not text:
                    continue
                baseline = round(y, 3)
                if baseline not in cells:
                    if page_no == 1 and y < 62:
                        continue  # title and column labels on first page
                    raise ExtractionError(f"Unassigned text on page {page_no}, y={y}: {text!r}")
                col = column_for(text, x, end)
                if cells[baseline][col] is not None:
                    raise ExtractionError(f"Two entries in {FIELDS[col]} on page {page_no}, y={y}.")
                cells[baseline][col] = text
            for y in sorted(cells):
                rows.append([v if v is not None else "" for v in cells[y]])
                locations.append((page_no, y))
            counts.append(len(cells))
            print(f"Page {page_no}: {len(cells)} rows", flush=True)
    return rows, counts, locations


def validate(rows, counts, locations):
    if len(counts) != EXPECTED_PAGES or len(rows) != EXPECTED_ROWS:
        raise ExtractionError(f"Expected {EXPECTED_PAGES} pages/{EXPECTED_ROWS} rows; "
                              f"found {len(counts)} pages/{len(rows)} rows.")
    missing = Counter()
    for row, (page, baseline) in zip(rows, locations):
        where = f"page {page}, y={baseline}, code={row[4]!r}"
        if not re.fullmatch(r"\d{9}[A-Z]\d{2}T?", row[4]):
            raise ExtractionError(f"Invalid school code: {where}")
        if not row[0] or not row[1] or not row[5] or not row[6]:
            raise ExtractionError(f"Missing required school field: {where}")
        for index in range(9, 12):
            # Remove only well-formed thousands separators, if present.
            if not re.fullmatch(r"(?:\d+|\d{1,3}(?:,\d{3})+)", row[index]):
                raise ExtractionError(f"Invalid pupil count in {FIELDS[index]}: {where}")
            row[index] = str(int(row[index].replace(",", "")))
        if int(row[9]) + int(row[10]) != int(row[11]):
            raise ExtractionError(f"Girls + boys does not equal total: {where}")
        for index in range(12, 19):
            if row[index] and not (0 <= float(row[index]) <= 100):
                raise ExtractionError(f"Indicator outside 0..100 in {FIELDS[index]}: {where}")
        missing.update(FIELDS[i] for i, value in enumerate(row) if value == "")
    departments = sorted({r[0] for r in rows})
    if len(departments) != 18:
        raise ExtractionError(f"Expected 18 departments; found {len(departments)}.")
    codes = Counter(r[4] for r in rows)
    return {
        "pages": len(counts), "rows": len(rows), "columns": len(FIELDS),
        "rows_per_page": counts, "departments": departments,
        "unique_school_codes": len(codes),
        "school_codes_with_T_suffix": sum(code.endswith("T") for code in codes),
        "pupil_sums": {FIELDS[i]: sum(int(r[i]) for r in rows) for i in (9, 10, 11)},
        "duplicate_school_codes": {code: count for code, count in sorted(codes.items()) if count > 1},
        "missing_by_column": {field: missing[field] for field in FIELDS},
        "pupil_totals_consistent": True,
        "indicator_values_within_0_100": True,
    }


def atomic_write(path, payload):
    path.parent.mkdir(parents=True, exist_ok=True)
    name = None
    try:
        with tempfile.NamedTemporaryFile(dir=path.parent, prefix=f".{path.name}.", delete=False) as f:
            name = f.name
            f.write(payload)
        os.replace(name, path)
    finally:
        if name and os.path.exists(name):
            os.unlink(name)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--project-root", type=Path,
                        default=Path(__file__).resolve().parents[2])
    parser.add_argument("--input", type=Path, help="Override the source PDF path.")
    parser.add_argument("--output", type=Path, help="Override the output CSV path.")
    parser.add_argument("--report", type=Path, help="Override the validation JSON path.")
    parser.add_argument("--overwrite", action="store_true",
                        help="Replace existing generated CSV/report after successful validation.")
    args = parser.parse_args()
    root = args.project_root.expanduser().resolve()
    source = (args.input or root / "data/raw/pdf/school_characteristics.pdf").expanduser().resolve()
    output = (args.output or root / "data/processed/csv/school_characteristics_extracted.csv").expanduser().resolve()
    report_path = (args.report or root / "data/processed/metadata/school_pdf_conversion_report.json").expanduser().resolve()
    if len({source, output, report_path, Path(__file__).resolve()}) != 4:
        parser.error("Input, CSV, report, and script paths must all be different.")
    if output.suffix.lower() != ".csv" or report_path.suffix.lower() != ".json":
        parser.error("Output must end in .csv; report must end in .json.")
    if not args.overwrite and (output.exists() or report_path.exists()):
        parser.error("A generated output already exists. Use --overwrite to regenerate it.")
    digest = hashlib.sha256(source.read_bytes()).hexdigest()
    if digest != SOURCE_SHA256:
        raise ExtractionError("Source PDF differs from the inspected original. "
                              "This layout-specific extractor must be reviewed for a different PDF.")
    if version("PyMuPDF") != "1.26.7":
        raise ExtractionError("Use PyMuPDF 1.26.7 as pinned in school_pdf_requirements.txt.")
    rows, counts, locations = extract_rows(source)
    report = validate(rows, counts, locations)
    buffer = io.StringIO(newline="")
    writer = csv.writer(buffer, lineterminator="\n")
    writer.writerow(FIELDS)
    writer.writerows(rows)
    payload = buffer.getvalue().encode("utf-8")
    report.update({
        "source_sha256": digest,
        "csv_sha256": hashlib.sha256(payload).hexdigest(),
        "script_sha256": hashlib.sha256(Path(__file__).read_bytes()).hexdigest(),
        "python_version": platform.python_version(),
        "dependencies": {"PyMuPDF": version("PyMuPDF")},
        "column_names": list(FIELDS),
        "notes": [
            "Source order and repeated school codes are preserved; no deduplication.",
            "Blank PDF cells are empty CSV fields; no zeros are imputed.",
            "Whitespace is collapsed to one space; spelling and accents are retained.",
            "Underlying PDF text that overflows a visible cell is retained in its original column.",
            "Structural validation is not verification against the original administrative database.",
            "This reconstruction has not been compared byte-for-byte with the earlier ChatGPT CSV.",
        ],
    })
    # No output is published until all pages and structural checks pass.
    atomic_write(output, payload)
    atomic_write(report_path, (json.dumps(report, ensure_ascii=False, indent=2) + "\n").encode("utf-8"))
    print(f"Saved {len(rows):,} rows and {len(FIELDS)} columns to {output}")
    print(f"Validation report: {report_path}")


if __name__ == "__main__":
    try:
        main()
    except (ExtractionError, OSError) as error:
        sys.exit(f"ERROR: {error}")
