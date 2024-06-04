# To make collage. Run on windows  visual studio. I suggest having a folder of only a few 100 images.
cd 'C:\Users\JR13\Documents\LOCAL_NOT_ONEDRIVE\rapid-plankton\edge-ai'
source env/scripts/activate
cd 'C:\Users\JR13\Documents\LOCAL_NOT_ONEDRIVE\rapid_paper\collage_maker-master'
python collage_maker.py -o copepods.png -w 1200 -i 250 -s -f 'C:\Users\JR13\Downloads\brawnybreakfasttrain\copepod'
python collage_maker.py -o detritus.png -w 1200 -i 250 -s -f 'C:\Users\JR13\Downloads\brawnybreakfasttrain\detritus'
python collage_maker.py -o noncopepod.png -w 1200 -i 250 -s -f 'C:\Users\JR13\Downloads\brawnybreakfasttrain\noncopepod'

