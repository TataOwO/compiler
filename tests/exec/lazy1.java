class Main {
    public static void main(String args[]) {
	boolean b = true || (1/0 == 1);
        if (b)
            System.out.print("true\n");
        else
            System.out.print("false\n");
        b = 1 == 2 && (1/0 == 1);
        if (b)
            System.out.print("true\n");
        else
            System.out.print("false\n");
    }
}

