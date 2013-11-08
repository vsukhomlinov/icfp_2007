package rna;

import com.sun.imageio.plugins.png.PNGImageWriter;
import com.sun.imageio.plugins.png.PNGImageWriterSpi;

import javax.imageio.stream.FileImageOutputStream;
import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
 * Created with IntelliJ IDEA.
 * User: vitaly
 * Date: 10/24/13
 * Time: 11:51 PM
 * To change this template use File | Settings | File Templates.
 */
public class TestFill {
    public static void main(String[] args) throws IOException {
        PNGImageWriter writer = new PNGImageWriter(new PNGImageWriterSpi());
        writer.setOutput(new FileImageOutputStream(new File("tmp/testFill.png")));
        BufferedImage image = new BufferedImage(200,200, BufferedImage.TYPE_INT_ARGB);

        Filler filler = new Filler(image);
        filler.fill(20,30,new Color(0,0,0));

        writer.write(image);
    }
}
