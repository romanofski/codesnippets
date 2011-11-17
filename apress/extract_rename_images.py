import argparse
import base64
import Image
import os
import lxml.html


PRINT_RESOLUTION = (150, 150)
IMG_FILENAME = 'Fig{0}.{1}'


def extract_images(xhtmlfile, chnumber):
    parsed = lxml.html.parse(xhtmlfile)
    curdir = os.path.dirname(xhtmlfile.name)
    imgdir = os.path.join(curdir, 'images')
    if not os.path.exists(imgdir):
        os.mkdir(imgdir)
    i = 0

    for img in parsed.xpath('//img'):
        i += 1
        imgdata = img.get('src')
        imgfilename = IMG_FILENAME.format(chnumber, i)
        data = base64.b64decode(imgdata[20:])

        # save the PNG data, reopen it *doh* and write the TIFF file
        open(os.path.join(imgdir, imgfilename + '.png'), 'w').write(data)
        Image.open(os.path.join(imgdir, imgfilename + '.png')).save(
            os.path.join(imgdir, imgfilename + '.tif'), 'TIFF',
            dpi=PRINT_RESOLUTION)
        os.unlink(os.path.join(imgdir, imgfilename + '.png'))
        print('Image written: {0}'.format(i))


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('file', type=file,
                        help='XHTML export of the chapter')
    parser.add_argument('chapter', type=int,
                        help='Chapter Number')
    p_args = parser.parse_args()
    extract_images(p_args.file, p_args.chapter)


if __name__ == "__main__":
    main()
