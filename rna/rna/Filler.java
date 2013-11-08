package rna;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: vitaly
 * Date: 10/15/13
 * Time: 10:18 PM
 * To change this template use File | Settings | File Templates.
 */
public class Filler {

    private BufferedImage bitmap;

    public Filler(BufferedImage bitmap) {
        this.bitmap = bitmap;
    }

    public void fill(int x, int y, Color color) {
        int initialColor = bitmap.getRGB(x, y);

//        draw(x,y,initialColor, color.getRGB());
        int newColor = color.getRGB();
        if(newColor != initialColor) {
            drawIncremental(x,y,initialColor, newColor);
        }
    }

    private void draw(int x, int y, int initialColor, int newColor) {
        if(x>=0 && x<600 && y>=0 && y<600) {
            int color = bitmap.getRGB(x,y);
            if (color == initialColor) {
                bitmap.setRGB(x,y,newColor);
                draw(x-1,y+1,initialColor, newColor);
                draw(x-1,y,initialColor, newColor);
                draw(x-1,y+1,initialColor, newColor);
                draw(x,y+1,initialColor, newColor);
                draw(x,y,initialColor, newColor);
                draw(x,y+1,initialColor, newColor);
                draw(x+1,y+1,initialColor, newColor);
                draw(x+1,y,initialColor, newColor);
                draw(x+1,y+1,initialColor, newColor);
            }
        }

    }

    private void drawIncremental(int x, int y, int initialColor, int newColor) {
        PositionFactory positionFactory = new PositionFactory(bitmap, initialColor);

        PositionFactory.Point pos = positionFactory.make(x, y);
        HashSet<PositionFactory.Point> toProcess = new HashSet<PositionFactory.Point>();

        toProcess.add(pos);

        HashSet<PositionFactory.Point> nextProcessed = new HashSet<PositionFactory.Point>();
        while(!toProcess.isEmpty()) {
            for(PositionFactory.Point p : toProcess) {
                bitmap.setRGB(p.x, p.y, newColor);
                nextProcessed.addAll(p.getFillableNeighbors());
            }
            toProcess.clear();
            toProcess.addAll(nextProcessed);
            nextProcessed.clear();
        }

    }

    private static class PositionFactory {
        private int sizeX;
        private int sizeY;
        private BufferedImage bitmap;
        private int initialColor;

        public PositionFactory(BufferedImage bitmap, int initialColor) {
            this.bitmap = bitmap;
            this.initialColor = initialColor;
            this.sizeY = bitmap.getHeight();
            this.sizeX = bitmap.getWidth();

        }

        private Point make(int x, int y) {
            return new Point(x,y);
        }

        private class Point {

            int x;
            int y;


            public Point(int x, int y) {
                this.x = x;
                this.y = y;
            }

            private boolean isFillable(int x, int y) {
                return bitmap.getRGB(x,y) == initialColor;
            }

            public List<Point> getFillableNeighbors() {
                ArrayList<Point> points = new ArrayList<Point>(4);
                if(x>=1 && isFillable(x-1,y)) {
                    points.add(new Point(x-1,y));
                }
                if(y>=1 && isFillable(x,y-1)) {
                    points.add(new Point(x,y-1));
                }
                if(x<sizeX-1 && isFillable(x+1,y)) {
                    points.add(new Point(x+1,y));
                }
                if(y<sizeY-1 && isFillable(x,y+1)) {
                    points.add(new Point(x,y+1));
                }
                return points;
            }

            @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (o == null || getClass() != o.getClass()) return false;

                Point point = (Point) o;

                return x == point.x && y == point.y;

            }

            @Override
            public int hashCode() {
                int result = x;
                result = 31 * result + y;
                return result;
            }
        }
    }




}
