package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;

import java.util.Objects;
import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BloodPressureFeatureSettingViewModel extends BaseCharacteristicViewModel {

    private final MutableLiveData<Boolean> isErrorResponse;

    private final MutableLiveData<Boolean> bodyMovementDetection;
    private final MutableLiveData<Boolean> cuffFitDetection;
    private final MutableLiveData<Boolean> irregularPulseDetection;
    private final MutableLiveData<Boolean> measurementPositionDetection;
    private final MutableLiveData<Boolean> pulseRateRangeDetection;
    private final MutableLiveData<Boolean> multipleBondDetection;

    private final MutableLiveData<String> responseDelay;
    private final MutableLiveData<String> responseDelayError;
    private final MutableLiveData<String> responseCode;
    private final MutableLiveData<String> responseCodeError;

    public BloodPressureFeatureSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        isErrorResponse = savedStateHandle.getLiveData("isErrorResponse");

        bodyMovementDetection = savedStateHandle.getLiveData("bodyMovementDetection");
        cuffFitDetection = savedStateHandle.getLiveData("cuffFitDetection");
        irregularPulseDetection = savedStateHandle.getLiveData("irregularPulseDetection");
        measurementPositionDetection = savedStateHandle.getLiveData("measurementPositionDetection");
        pulseRateRangeDetection = savedStateHandle.getLiveData("pulseRateRangeDetection");
        multipleBondDetection = savedStateHandle.getLiveData("multipleBondDetection");

        responseDelay = savedStateHandle.getLiveData("responseDelay");
        responseDelayError = savedStateHandle.getLiveData("responseDelayError");
        responseCode = savedStateHandle.getLiveData("responseCode");
        responseCodeError = savedStateHandle.getLiveData("responseCodeError");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mCharacteristicData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString())))
                    .subscribeOn(Schedulers.io())
                    .observeOn(Schedulers.io())
                    .flatMapCompletable(dataString -> {
                        if (dataString.isPresent()) {
                            try {
                                mCharacteristicData = mGson.fromJson(dataString.get(), CharacteristicData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        if (mCharacteristicData == null) {
                            mCharacteristicData = new CharacteristicData();
                            mCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
                            mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
                            mCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
                        }

                        if (isErrorResponse.getValue() == null) {
                            isErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
                        }

                        BloodPressureFeature bloodPressureFeature;
                        if (mCharacteristicData.data != null) {
                            bloodPressureFeature = new BloodPressureFeature(mCharacteristicData.data);
                        } else {
                            bloodPressureFeature = null;
                        }

                        if (bloodPressureFeature == null) {
                            if (bodyMovementDetection.getValue() == null) {
                                bodyMovementDetection.postValue(false);
                            }
                            if (cuffFitDetection.getValue() == null) {
                                cuffFitDetection.postValue(false);
                            }
                            if (irregularPulseDetection.getValue() == null) {
                                irregularPulseDetection.postValue(false);
                            }
                            if (measurementPositionDetection.getValue() == null) {
                                measurementPositionDetection.postValue(false);
                            }
                            if (pulseRateRangeDetection.getValue() == null) {
                                pulseRateRangeDetection.postValue(false);
                            }
                            if (multipleBondDetection.getValue() == null) {
                                multipleBondDetection.postValue(false);
                            }
                        } else {
                            if (bodyMovementDetection.getValue() == null) {
                                bodyMovementDetection.postValue(bloodPressureFeature.isBodyMovementDetectionSupported());
                            }
                            if (cuffFitDetection.getValue() == null) {
                                cuffFitDetection.postValue(bloodPressureFeature.isCuffFitDetectionSupported());
                            }
                            if (irregularPulseDetection.getValue() == null) {
                                irregularPulseDetection.postValue(bloodPressureFeature.isIrregularPulseDetectionSupported());
                            }
                            if (measurementPositionDetection.getValue() == null) {
                                measurementPositionDetection.postValue(bloodPressureFeature.isMeasurementPositionDetectionSupported());
                            }
                            if (pulseRateRangeDetection.getValue() == null) {
                                pulseRateRangeDetection.postValue(bloodPressureFeature.isPulseRateRangeDetectionSupported());
                            }
                            if (multipleBondDetection.getValue() == null) {
                                multipleBondDetection.postValue(bloodPressureFeature.isMultipleBondSupported());
                            }
                        }

                        String text = String.valueOf(mCharacteristicData.delay);
                        if (responseDelay.getValue() == null) {
                            responseDelay.postValue(text);
                            responseDelayError.postValue(mResourceTextSource.getResponseDelayErrorString(text));
                        }

                        text = String.valueOf(mCharacteristicData.responseCode);
                        if (responseCode.getValue() == null) {
                            responseCode.postValue(text);
                            responseCodeError.postValue(mResourceTextSource.getResponseCodeErrorString(text));
                        }

                        return Completable.complete();
                    });
        } else {
            completable = Completable.complete();
        }
        return completable;
    }

    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(isErrorResponse).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIsErrorResponse(@NonNull Boolean checked) {
        isErrorResponse.setValue(checked);
    }

    public void observeBodyMovementDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(bodyMovementDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateBodyMovementDetection(@NonNull Boolean text) {
        bodyMovementDetection.setValue(text);
    }

    public void observeCuffFitDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(cuffFitDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateCuffFitDetection(@NonNull Boolean text) {
        cuffFitDetection.setValue(text);
    }

    public void observeIrregularPulseDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(irregularPulseDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIrregularPulseDetection(@NonNull Boolean text) {
        irregularPulseDetection.setValue(text);
    }

    public void observeMeasurementPositionDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(measurementPositionDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateMeasurementPositionDetection(@NonNull Boolean text) {
        measurementPositionDetection.setValue(text);
    }

    public void observePulseRateRangeDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(pulseRateRangeDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updatePulseRateRangeDetection(@NonNull Boolean text) {
        pulseRateRangeDetection.setValue(text);
    }

    public void observeMultipleBondDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(multipleBondDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateMultipleBondDetection(@NonNull Boolean text) {
        multipleBondDetection.setValue(text);
    }

    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseDelay).observe(owner, observer);
    }

    public void observeResponseDelayError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(responseDelayError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateResponseDelay(@NonNull CharSequence text) {
        responseDelay.setValue(text.toString());
        responseDelayError.setValue(mResourceTextSource.getResponseDelayErrorString(text));
    }

    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(responseCode).observe(owner, observer);
    }

    public void observeResponseCodeError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(responseCodeError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateResponseCode(@NonNull CharSequence text) {
        responseCode.setValue(text.toString());
        responseCodeError.setValue(mResourceTextSource.getResponseCodeErrorString(text));
    }

    @NonNull
    @Override
    public Single<Optional<Intent>> save() {
        Intent intent;
        if ((Boolean.TRUE.equals(isErrorResponse.getValue())
                && TextUtils.isEmpty(responseCodeError.getValue())
                || Boolean.FALSE.equals(isErrorResponse.getValue()))
                && TextUtils.isEmpty(responseDelayError.getValue())) {
            mCharacteristicData.data = new BloodPressureFeature(Objects.requireNonNull(bodyMovementDetection.getValue())
                    , Objects.requireNonNull(cuffFitDetection.getValue())
                    , Objects.requireNonNull(irregularPulseDetection.getValue())
                    , Objects.requireNonNull(pulseRateRangeDetection.getValue())
                    , Objects.requireNonNull(measurementPositionDetection.getValue())
                    , Objects.requireNonNull(multipleBondDetection.getValue())
                    , false
                    , false
                    , false)
                    .getBytes();
            if (Boolean.TRUE.equals(isErrorResponse.getValue())) {
                mCharacteristicData.responseCode = Integer.parseInt(Objects.requireNonNull(responseCode.getValue()));
            } else {
                mCharacteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;
            }
            mCharacteristicData.delay = Long.parseLong(Objects.requireNonNull(responseDelay.getValue()));

            intent = new Intent();
            intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }

}