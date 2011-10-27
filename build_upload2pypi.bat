rem set HOME=C:\Users\Owner\
cd E:\dao
rem d:\Python24\python.exe setup.py register
rem d:\Python24\python.exe setup.py bdist_egg upload --quiet
rem d:\Python25\python.exe setup.py bdist_egg upload --quiet
d:\Python26\python.exe setup.py bdist_egg upload --quiet
rem d:\Python24\python.exe setup.py bdist_wininst register upload --quiet
rem d:\Python25\python.exe setup.py bdist_wininst register upload --quiet
d:\Python26\python.exe setup.py bdist_wininst  --target-version=2.6 register upload --quiet
d:\Python26\python.exe setup.py sdist upload
d:\Python26\python.exe setup.py build_sphinx
d:\Python26\python.exe setup.py upload_sphinx
pause