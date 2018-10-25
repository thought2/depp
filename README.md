# depp

## Creates a .dot file showing a dependency graph of your project


### Installation instructions for a Debian based Linux system 

```

# install graphviz and depp: 
sudo apt-get install graphviz
npm install -g @thought2/depp

cd {{project_dir}}

# generates the dot file:
LANGUAGE=elm DIR=src/ MAIN=Main.elm depp > output.dot

# turns dot file into png:
dot -Tpng output.dot > output.png

```

Currently `depp` is only implemented for Elm. But it's extensible, feel free to make a PR for the support of further languages.
