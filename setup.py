from setuptools import setup, find_packages

setup(
  name = "daot",
  version = "0.7.2",
  packages = find_packages(),
##  scripts = ['say_hello.py'],

##  # Project uses reStructuredText, so ensure that the docutils get
##  # installed or upgraded on the target machine
##  install_requires = ['docutils>=0.3'],

  package_data = {
##      '': ['*.txt', '*.rst'] #,
      r'document\_build\html':['*.*'],
      r'document\zh':['*.rst', '*.html', '*.js'],
      r'document\en':['*.rst', '*.html', '*.js']
  },

  author='Cao Xingming(Simeon.Chaos)',
  author_email='simeon.chaos@gmail.com',
  license='GPL',
  url='http://code.google.com/p/daot',
  download_url='http://code.google.com/p/daot',
  keywords = "dao dinpy lisp prolog dsl",

  description='the dao to programming',
  long_description=open('readme').read(),
  
  platforms='Posix; MacOS X; Windows',
  classifiers = ['Development Status :: 3 - Alpha',
               'License :: OSI Approved :: GNU General Public License (GPL)',
               'Natural Language :: Chinese (Simplified)',
               'Programming Language :: Python',
               'Topic :: Software Development :: Compilers',
               'Topic :: Text Processing :: General',
               'Intended Audience :: Developers']
)