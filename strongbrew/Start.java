package strongbrew;

import strongbrew.os.env;
import main;

public class Start {
    public static void main(String[] args) {
        env.setArgs(args);
        main.main();
    }
}
