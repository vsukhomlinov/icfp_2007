package rna;

import java.util.HashMap;
import java.util.Map;

public enum RNA {
    ADD_BLK("PIPIIIC"),
    ADD_RED("PIPIIIP"),
    ADD_GRN("PIPIICC"),
    ADD_YLW("PIPIICF"),
    ADD_BLU("PIPIICP"),
    ADD_MGN("PIPIIFC"),
    ADD_CYA("PIPIIFF"),
    ADD_WHT("PIPIIPC"),
    ADD_TRS("PIPIIPF"),
    ADD_OPQ("PIPIIPP"),

    EMP_BUK("PIIPICP"),

    MOVE   ("PIIIIIP"),
    TRN_CCW("PCCCCCP"),
    TRN_CW ("PFFFFFP"),

    MARK   ("PCCIFFP"),
    LINE   ("PFFICCP"),
    FILL   ("PIIPIIP"),

    NEW_BMP("PCCPFFP"),
    COMPOSE("PFFPCCP"),
    CLIP   ("PFFICCF");


    private String code;


    RNA(String code) {
        this.code = code;
    }

    private static final Map<String, RNA> map = new HashMap<String, RNA>();

    static {
        for (RNA type : RNA.values()) {
            map.put(type.code, type);
        }
    }

    public static RNA fromString(String code) {
        return map.get(code);
    }
}