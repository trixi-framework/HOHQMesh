# Building the Documentation

To generate the HOHQMesh documentation, you need a reasonably recent version of
Python 3 and the [`mkdocs-material`](https://squidfunk.github.io/mkdocs-material/)
package. The latter can be installed via `pip` using
```bash
pip install mkdocs-material
```
which will also install all relevant dependencies. Note that on some
systems (e.g., Ubuntu 20.04), `mkdocs-material` requires a minimum version of
the dependent `Jinja2` package that has a known bug. Please make sure that you
have *at least* `Jinja2 2.11` installed by checking the output of
```bash
pip show Jinja2
```
If it is too old, you can upgrade Jinja2 by running
```bash
pip install Jinja2 --upgrade
```

For local viewing, change to the directory where the `mkdocs.yml` file is
located. Then execute `./preparedocs` to prepare and process some markdown files
that are located out of the main documentation directory `docs`. Then, you have
two options:

To view the documentation via the built-in webserver, execute
```bash
mkdocs serve
```
and follow the link printed to the terminal to open the documentation in your browser.

To generate a static set of files on disk, run
```bash
mkdocs build --no-directory-urls
```
and open the file `site/index.html` in your browser.
