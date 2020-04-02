import java.net.*;
import java.net.http.*;
import java.util.*;

class Main {
  public static void main(String[] args) throws Exception {
    System.out.println("starting");
    var http = HttpClient.newHttpClient();
    var token = args[0];
    var req = HttpRequest.newBuilder()
                  .uri(URI.create("https://discordapp.com/api/gateway/bot"))
                  .header("Authorization", "Bot " + token)
                  .build();
    int s = 0;
    while (true) {
      Thread.sleep(5000);
      s += 5;
      var res = http.send(req, HttpResponse.BodyHandlers.ofString());
      System.out.println("status code " + res.statusCode());
      System.out.println(s);
    }
  }
}
