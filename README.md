# XTriples

[![DOI](https://zenodo.org/badge/24633761.svg)](https://zenodo.org/badge/latestdoi/24633761)

## A generic webservice to extract RDF statements from XML resources.

Check out the demo and documentation at https://xtriples.lod.academy

With this webservice you can crawl XML repositories and extract RDF statements 
using a simple configuration based on XPATH expressions.

![Structure of the XTriples webservice](resources/images/structure.png "XTriples structure")

This repository contains the source code of the XTriples webservice.

The webservice is distributed as a single .xar file, which can be installed into an eXist-db instance via the package manager.

You can download the latest version from GitHub: https://github.com/digicademy/xtriples/releases/latest

Read the documentation on how to setup your own instance of XTriples: http://xtriples.lod.academy/documentation.html#setup

## Building

To build the .xar yourself, you need to have Apache Ant installed. Alternatively you can use the Ant version which comes with eXist-db 
(call build.sh/build.bat from the eXist-db directory). 

Clone this repository to a local directory:

	git clone https://github.com/digicademy/xtriples.git xtriples

Change into the created directory and call

	ant

This should create a .xar file in the build directory.

## License

This software is published under the terms of the MIT license.

## Research Software Engineering and Development

Copyright 2015-2019, <a href="https://orcid.org/0000-0002-0953-2818">Torsten Schrade</a>  
Copyright 2015-2019, <a href="http://www.adwmainz.de/">Academy of Sciences and Literature | Mainz</a>

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
