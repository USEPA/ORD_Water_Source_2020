import pdfplumber

with pdfplumber.open(r'D:\example.pdf') as pdf:
    first_page = pdf.pages[0]
    print(first_page.extract_text())
    
    
# Tesseract
from PIL import Image
import pytesseract
import numpy as np

pytesseract.pytesseract.tesseract_cmd = r'c:\users\amurra02\appdata\local\pip\cache\wheels\c6\3a\30\877d14dc50fb68f107b18247a31db742518c6f74f64de8dde8\pytesseract-0.3.8-py2.py3-none-any.whl'

filename = 'D:\example.png'
img1 = np.array(Image.open(filename))
text = pytesseract.image_to_string(img1)
