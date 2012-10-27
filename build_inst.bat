rem set HOME=C:\Users\Owner\
cd E:\dao
rem d:\Python24\python.exe setup.py bdist_egg
rem d:\Python25\python.exe setup.py bdist_egg
d:\Python26\python.exe setup.py bdist_egg
rem d:\Python24\python.exe setup.py bdist_wininst
rem d:\Python25\python.exe setup.py bdist_wininst
d:\Python26\python.exe setup.py bdist_wininst --target-version=2.6
d:\Python26\python.exe setup.py sdist
d:\Python26\python.exe setup.py build_sphinx
pause