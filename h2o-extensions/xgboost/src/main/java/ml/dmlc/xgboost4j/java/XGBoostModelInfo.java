package ml.dmlc.xgboost4j.java;

import hex.DataInfo;
import hex.tree.xgboost.XGBoostModel;
import water.Iced;
import water.Key;
import water.util.Log;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.Arrays;


/**
 * This class contains the state of the Deep Learning model
 * This will be shared: one per node
 */
final public class XGBoostModelInfo extends Iced {
  public final XGBoostModel.XGBoostParameters _parameters; // not used, kept for debugging purposes
  public final Key<DataInfo> _dataInfoKey;

  public String _featureMap;
  public byte[] _boosterBytes; // internal state of native backend

  /**
   * Main constructor
   * @param origParams Model parameters
   */
  public XGBoostModelInfo(final XGBoostModel.XGBoostParameters origParams, DataInfo dinfo) {
    _parameters = (XGBoostModel.XGBoostParameters) origParams.clone(); //make a copy, don't change model's parameters
    _dataInfoKey = dinfo._key;
  }

  public String getFeatureMap() {
    return _featureMap;
  }

  public void setFeatureMap(String featureMap) {
    _featureMap = featureMap;
  }

  public void setBoosterBytes(byte[] boosterBytes) {
    _boosterBytes = boosterBytes;
  }

  public Booster deserializeBooster() {
    if (_boosterBytes == null) {
      throw new IllegalStateException("Booster not initialized!");
    }
    try {
      Booster booster = Booster.loadModel(new ByteArrayInputStream(_boosterBytes));
      Log.debug("Booster created from bytes, raw size = " + _boosterBytes.length);
      return booster;
    } catch (XGBoostError | IOException exception) {
      throw new IllegalStateException("Failed to load the booster.", exception);
    }
  }

  @Override
  public int hashCode() {
    return Arrays.hashCode(_boosterBytes);
  }

  // compute model size (number of model parameters required for making predictions)
  // momenta are not counted here, but they are needed for model building
  public long size() {
    long res = 0;
    if (_boosterBytes !=null) res+= _boosterBytes.length;
    return res;
  }

  public DataInfo dataInfo() {
    return _dataInfoKey.get();
  }

}
