/* Command lines
javac OracleBulkInsert.java
java -cp "ojdbc7.jar;" OracleBulkInsert
*/

import java.io.*;
import java.sql.*;

public class OracleBulkInsert {
    public static void main(String[] args) throws SQLException {
        try {
            long startTime = System.nanoTime();
            int Count = 0;

            String url = "jdbc:oracle:thin:@//127.0.0.1:1521/dbservice";
            Connection conn = DriverManager.getConnection(url, "user", "password");

            PreparedStatement ps = conn.prepareStatement("insert into test1(ITEM) values (:ITEM)");

            BufferedReader bf = new BufferedReader(new FileReader("bulkFile.txt"));
            String line = null;
            do {
                line = bf.readLine();
                if (line != null) {
                    ps.setString(1, line);
                    ps.addBatch();

                    Count++;
                    if (Count % 10000 == 0) {
                        ps.executeBatch();
                    }

                    if (Count % 100000 == 0) {
                        System.out.println("processed " + Count);
                    }
                }
            } while (line != null);
            bf.close();
            ps.executeBatch();
            ps.close();
            conn.close();

            long FN = System.nanoTime() - startTime;
            long FMs = FN / 1000000;
            long FSc = FMs / 1000;
            long FMn = FSc / 60;
            long Hour = FMn / 60;
            long Minute = FMn % 60;
            long Second = FSc % 60;
            long Milli = FMs % 1000;
            // long Nano = FN % 1000000;
            System.out.printf("%d strings inserted in %02d:%02d:%02d.%d%n", Count, Hour, Minute, Second, Milli);
        } catch (Exception e) {
            System.out.println(e);
        }
    }
}
