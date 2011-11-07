import argparse
import base64
import os
import lxml.html


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
        imgfilename = 'Fig{0}.{1}.png'.format(chnumber, i)
        data = base64.b64decode(imgdata[20:])
        open(os.path.join(imgdir, imgfilename), 'w').write(data)
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
