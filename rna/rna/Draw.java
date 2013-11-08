package rna;

import com.sun.imageio.plugins.png.PNGImageWriter;
import com.sun.imageio.plugins.png.PNGImageWriterSpi;

import javax.imageio.stream.FileImageOutputStream;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.ImageObserver;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;
import java.util.logging.Logger;

import static rna.BucketColor.*;


/**
 * Created with IntelliJ IDEA.
 * User: vitaly
 * Date: 10/14/13
 * Time: 11:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class Draw {

    public static final int IMAGE_SIZE = 600;
    private List<Color> bucket = new ArrayList<Color>();
    private List<Integer> transparency_bucket = new ArrayList<Integer>();

    private Position pos = new Position(0,0);
    private Position mark = new Position(0,0);

    private Stack<BufferedImage> bitmaps = new Stack<BufferedImage>();
    private Graphics2D graphics;

    private static Logger LOG = Logger.getLogger("Draw");

    public Draw() {
        peekImage();
    }


    public void draw(RNA chunk) {
//        System.out.println(chunk);
        switch (chunk) {
            case ADD_BLK: addBucket(BLACK); break;
            case ADD_WHT: addBucket(WHITE); break;
            case ADD_RED: addBucket(RED); break;
            case ADD_GRN: addBucket(GREEN); break;
            case ADD_BLU: addBucket(BLUE); break;
            case ADD_CYA: addBucket(CYAN); break;
            case ADD_MGN: addBucket(MAGENTA); break;
            case ADD_YLW: addBucket(YELLOW); break;

            case ADD_TRS: transparency_bucket.add(0); break;
            case ADD_OPQ: transparency_bucket.add(255); break;

            case EMP_BUK: bucket.clear(); transparency_bucket.clear(); break;

            case MOVE: pos.move();break;
            case TRN_CCW: pos.turnCCW();break;
            case TRN_CW: pos.turnCW();break;

            case MARK: mark = pos.copy();break;
            case LINE: line(pos,mark);break;
            case FILL: fill(pos);break;
            case NEW_BMP: peekImage();break;
            case COMPOSE: compose();break;
            case CLIP: clip();break;



        }

    }

    private void addBucket(Color color) {
//        System.out.println("Add " + color.toString());
        bucket.add(color);
    }

    private void clip() {
        System.out.println("CLIP");
        throw new RuntimeException("Not implemented");
    }

    private void compose() {
        System.out.println("Compose "+bitmaps.size());
        if(bitmaps.size()>1) {
            BufferedImage from = bitmaps.pop();
            BufferedImage to = bitmaps.peek();
            to.getGraphics().drawImage(from, 0, 0, new NoOpObserver());
            graphics = to.createGraphics();
        }
    }

    private void peekImage() {
        if(bitmaps.size()<10) {
            BufferedImage image = new BufferedImage(IMAGE_SIZE, IMAGE_SIZE, BufferedImage.TYPE_INT_ARGB);
            bitmaps.push(image);
            graphics = image.createGraphics();
        }
    }

    private void fill(Position pos) {
        Color color = getColor();
        System.out.println(String.format("fill %s with %s@%s", pos, color, color.getAlpha()));
        new Filler(bitmaps.peek()).fill(pos.x, pos.y, color);
    }

    private void line(Position from, Position to) {
//        System.out.println(String.format("Line: (%s,%s)-(%s,%s) @ %s", from.x,from.y, to.x,to.y,getColor()));
        Graphics2D bitmap = graphics;
        bitmap.setColor(getColor());
        bitmap.drawLine(from.x, from.y, to.x, to.y);
    }

    private int getOpacity() {
        if (transparency_bucket.isEmpty()) {
            return 255;
        }
        int sum = 0;
        for (int c : transparency_bucket) {
            sum += c;
        }
        return sum/transparency_bucket.size();
    }

    private Color getColor() {
        int opacity = getOpacity();
        if (bucket.isEmpty()) {
            return new Color(0,0,0,opacity);
        }
        int ra=0;
        int ga=0;
        int ba=0;
        for(Color c:bucket) {
            ra += c.getRed();
            ga += c.getGreen();
            ba += c.getBlue();
        }
        int divider = bucket.size()*255;
        return new Color(ra * opacity / divider, ga * opacity / divider, ba * opacity / divider, opacity);
    }

    public void drawFile(File file) throws IOException {
        if(!file.exists()) {
            file.createNewFile();
        }
        PNGImageWriter writer = new PNGImageWriter(new PNGImageWriterSpi());
        writer.setOutput(new FileImageOutputStream(file));
        BufferedImage image = bitmaps.peek();
        int[] alphaPixels = new int[image.getWidth()*image.getHeight()];
        Arrays.fill(alphaPixels, 255);
        image.getAlphaRaster().setPixels(0,0,image.getWidth(),image.getHeight(), alphaPixels);
        writer.write(image);
    }

    private class NoOpObserver implements ImageObserver {

        @Override
        public boolean imageUpdate(Image img, int infoflags, int x, int y, int width, int height) {
            return false;
        }
    }


}