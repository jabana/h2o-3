package hex.genmodel;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public abstract class MultiModelMojoReader<M extends MojoModel> extends ModelMojoReader<M> {

  private Map<String, MojoModel> _subModels;

  @Override
  protected final void readModelData() throws IOException {
    int subModelCount = readkv("submodel_count", 0);
    HashMap<String, MojoModel> models = new HashMap<>(subModelCount);
    for (int i = 0; i < subModelCount; i++) {
      String key = readkv("submodel_key_" + i);
      String zipDirectory = readkv("submodel_dir_" + i);
      MojoModel model = ModelMojoReader.readFrom(new NestedMojoReaderBackend(zipDirectory));
      models.put(key, model);
    }
    _subModels = Collections.unmodifiableMap(models);
    readParentModelData();
    MojoModel[] mojoModels = new MojoModel[models.size()];
    models.values().toArray(mojoModels);
    reorder(mojoModels, readkv("base_models_num", 0));
  }


  private void reorder(final MojoModel[] models, final int baseModelNum) {
    final MojoModel reference = models[baseModelNum];

    for (int i = 0; i < models.length; i++) {
      final String[] names = models[i]._names;
      for (int j = 0; j < reference._names.length; j++) {
        if (!names[j].equals(reference._names[j])) {
          reorder(models[i], reference._names);
          break;
        }
      }
    }
  }

  private void reorder(final MojoModel model, final String[] referentialNames) {
    String[] originalNames = Arrays.copyOf(model._names, model._names.length);
    String[][] originalDomains = Arrays.copyOf(model._domains, model._domains.length);

    for (int i = 0; i < referentialNames.length; i++) {
      final int pos = findIndex(originalNames, referentialNames[i]);
      assert pos != -1; // Names of columns should be the same. If not, at least one of the models is broken.

      model._names[i] = originalNames[pos];
      model._domains[i] = originalDomains[pos];
    }

  }

  private int findIndex(String[] arr, String searchedVal) {

    for (int i = 0; i < arr.length; i++) {
      if (arr[i].equals(searchedVal)) return i;
    }

    return -1;
  }

  protected MojoModel getModel(String key) {
    return _subModels.get(key);
  }

  protected Map<String, MojoModel> getSubModels() {
    return _subModels;
  }

  protected abstract void readParentModelData() throws IOException;

  private class NestedMojoReaderBackend implements MojoReaderBackend {

    private String _zipDirectory;

    private NestedMojoReaderBackend(String zipDirectory) {
      _zipDirectory = zipDirectory;
    }

    @Override
    public BufferedReader getTextFile(String filename) throws IOException {
      return _reader.getTextFile(_zipDirectory + filename);
    }

    @Override
    public byte[] getBinaryFile(String filename) throws IOException {
      return _reader.getBinaryFile(_zipDirectory + filename);
    }

    @Override
    public boolean exists(String filename) {
      return _reader.exists(_zipDirectory + filename);
    }
  }

}
