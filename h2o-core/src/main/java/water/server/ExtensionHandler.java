package water.server;

import water.AbstractH2OExtension;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Extension point for HTTP request handling.
 */
public interface ExtensionHandler {
  /**
   * Extended request handler
   * @param target -
   * @param request -
   * @param response -
   * @return true if the request should be processed handled, false otherwise
   * @throws IOException -
   * @throws ServletException -
   */
  boolean handle(String target, HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException;

  /**
   * Static registry of handler extensions.
   * Customizations can use this to inject their handlers here during {@link AbstractH2OExtension#init()}.
   */
  Registry REGISTRY = new Registry();

  class Registry {
    private final List<ExtensionHandler> extensionHandlers = new ArrayList<>();

    public void add(ExtensionHandler extensionHandler) {
      extensionHandlers.add(extensionHandler);
    }

    public Iterable<ExtensionHandler> getAll() {
      return extensionHandlers;
    }
  }
}
