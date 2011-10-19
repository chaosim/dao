from setuptools import setup, find_packages

setup(
  name = "daot",
  version = "0.7.0",
  packages = find_packages(),
##  scripts = ['say_hello.py'],

##  # Project uses reStructuredText, so ensure that the docutils get
##  # installed or upgraded on the target machine
##  install_requires = ['docutils>=0.3'],

  package_data = {
      '': ['*.txt', '*.rst'] #,
##      'dao':['*.pdf']
  },

  author='Cao Xingming(Simeon.Chaos)',
  author_email='simeon.chaos@gmail.com',
  license='GPL',
  url='http://code.google.com/p/daot',
  download_url='http://code.google.com/p/daot',
  keywords = "dao dinpy lisp prolog dsl",

  description='the dao to programming',
  long_description='dao: the new generation programming language '
                   'implemented by a functional logic evaluator, '
                   'unifying code with data, grammar with program, '
                   'logic with functional, compiling with running.',
  
  platforms='Posix; MacOS X; Windows',
  classifiers = ['Development Status :: 3 - Alpha',
               'License :: OSI Approved :: GNU General Public License (GPL)',
               'Natural Language :: Chinese (Simplified)',
               'Programming Language :: Python',
               'Topic :: Software Development :: Compilers',
               'Topic :: Text Processing :: General',
               'Intended Audience :: Developers']
)