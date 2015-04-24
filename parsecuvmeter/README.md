*NOTE:* This is currently hard coded to Brisbane, and used as a sample
only.

Quick hack to test a UVMeter implementation using parsec. The UVMeter is
a new plug-in for xmobar (see https://github.com/jaor/xmobar)

Thanks to
http://charlieharvey.org.uk/page/naive_xml_parser_with_haskell_parsec_and_perl_regexen_part_one_haskell/
for the great introduction on how to use parsec.

If you want to try out this code run:

    $ cabal sandbox init
    $ cabal install
    $ wget http://www.arpansa.gov.au/uvindex/realtime/xml/uvvalues.xml
    $ cat uvvalues.xml | .cabal-sandbox/bin/uvmeter
