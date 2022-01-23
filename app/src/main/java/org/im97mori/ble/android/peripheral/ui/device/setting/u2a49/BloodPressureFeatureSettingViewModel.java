package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import static org.im97mori.ble.android.peripheral.utils.Utils.stackLog;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class BloodPressureFeatureSettingViewModel extends BaseCharacteristicViewModel {

    private static final String KEY_IS_ERROR_RESPONSE = "KEY_IS_ERROR_RESPONSE";

    private static final String KEY_BODY_MOVEMENT_DETECTION = "KEY_BODY_MOVEMENT_DETECTION";
    private static final String KEY_CUFF_FIT_DETECTION = "KEY_CUFF_FIT_DETECTION";
    private static final String KEY_IRREGULAR_PULSE_DETECTION = "KEY_IRREGULAR_PULSE_DETECTION";
    private static final String KEY_PULSE_RATE_RANGE_DETECTION = "KEY_PULSE_RATE_RANGE_DETECTION";
    private static final String KEY_MEASUREMENT_POSITION_DETECTION = "KEY_MEASUREMENT_POSITION_DETECTION";
    private static final String KEY_MULTIPLE_BOND = "KEY_MULTIPLE_BOND";

    private static final String KEY_RESPONSE_CODE = "KEY_RESPONSE_CODE";
    private static final String KEY_RESPONSE_DELAY = "KEY_RESPONSE_DELAY";

    private final MutableLiveData<Boolean> mIsErrorResponse;

    private final MutableLiveData<Boolean> mBodyMovementDetection;
    private final MutableLiveData<Boolean> mCuffFitDetection;
    private final MutableLiveData<Boolean> mIrregularPulseDetection;
    private final MutableLiveData<Boolean> mPulseRateRangeDetection;
    private final MutableLiveData<Boolean> mMeasurementPositionDetection;
    private final MutableLiveData<Boolean> mMultipleBond;

    private final MutableLiveData<String> mResponseCode;
    private final MutableLiveData<String> mResponseDelay;

    private final MutableLiveData<Intent> mSavedData;

    @Inject
    public BloodPressureFeatureSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        super(deviceSettingRepository, gson);

        mIsErrorResponse = savedStateHandle.getLiveData(KEY_IS_ERROR_RESPONSE);

        mBodyMovementDetection = savedStateHandle.getLiveData(KEY_BODY_MOVEMENT_DETECTION);
        mCuffFitDetection = savedStateHandle.getLiveData(KEY_CUFF_FIT_DETECTION);
        mIrregularPulseDetection = savedStateHandle.getLiveData(KEY_IRREGULAR_PULSE_DETECTION);
        mPulseRateRangeDetection = savedStateHandle.getLiveData(KEY_PULSE_RATE_RANGE_DETECTION);
        mMeasurementPositionDetection = savedStateHandle.getLiveData(KEY_MEASUREMENT_POSITION_DETECTION);
        mMultipleBond = savedStateHandle.getLiveData(KEY_MULTIPLE_BOND);

        mResponseCode = savedStateHandle.getLiveData(KEY_RESPONSE_CODE);
        mResponseDelay = savedStateHandle.getLiveData(KEY_RESPONSE_DELAY);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    @Override
    public void observeSetup(@NonNull Intent intent
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                try {
                    mCharacteristicData = mGson.fromJson(intent.getStringExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString())
                            , CharacteristicData.class);
                } catch (JsonSyntaxException e) {
                    stackLog(e);
                }

                if (mCharacteristicData == null) {
                    mCharacteristicData = new CharacteristicData();
                    mCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
                    mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
                    mCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
                }
            }

            if (mIsErrorResponse.getValue() == null) {
                mIsErrorResponse.postValue(mCharacteristicData.responseCode != BluetoothGatt.GATT_SUCCESS);
            }

            BloodPressureFeature bloodPressureFeature;
            if (mCharacteristicData.data == null) {
                bloodPressureFeature = null;
            } else {
                bloodPressureFeature = new BloodPressureFeature(mCharacteristicData.data);
            }

            if (mBodyMovementDetection.getValue() == null) {
                if (bloodPressureFeature == null) {
                    mBodyMovementDetection.postValue(false);
                } else {
                    mBodyMovementDetection.postValue(bloodPressureFeature.isBodyMovementDetectionSupported());
                }
            }

            if (mCuffFitDetection.getValue() == null) {
                if (bloodPressureFeature == null) {
                    mCuffFitDetection.postValue(false);
                } else {
                    mCuffFitDetection.postValue(bloodPressureFeature.isCuffFitDetectionSupported());
                }
            }

            if (mIrregularPulseDetection.getValue() == null) {
                if (bloodPressureFeature == null) {
                    mIrregularPulseDetection.postValue(false);
                } else {
                    mIrregularPulseDetection.postValue(bloodPressureFeature.isIrregularPulseDetectionSupported());
                }
            }

            if (mPulseRateRangeDetection.getValue() == null) {
                if (bloodPressureFeature == null) {
                    mPulseRateRangeDetection.postValue(false);
                } else {
                    mPulseRateRangeDetection.postValue(bloodPressureFeature.isPulseRateRangeDetectionSupported());
                }
            }

            if (mMeasurementPositionDetection.getValue() == null) {
                if (bloodPressureFeature == null) {
                    mMeasurementPositionDetection.postValue(false);
                } else {
                    mMeasurementPositionDetection.postValue(bloodPressureFeature.isMeasurementPositionDetectionSupported());
                }
            }

            if (mMultipleBond.getValue() == null) {
                if (bloodPressureFeature == null) {
                    mMultipleBond.postValue(false);
                } else {
                    mMultipleBond.postValue(bloodPressureFeature.isMultipleBondSupported());
                }
            }

            if (mResponseDelay.getValue() == null) {
                mResponseDelay.postValue(String.valueOf(mCharacteristicData.delay));
            }

            if (mResponseCode.getValue() == null) {
                mResponseCode.postValue(String.valueOf(mCharacteristicData.responseCode));
            }

            emitter.onComplete();
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeIsErrorResponse(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsErrorResponse).observe(owner, observer);
    }

    @MainThread
    public void observeBodyMovementDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBodyMovementDetection).observe(owner, observer);
    }

    @MainThread
    public void observeCuffFitDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mCuffFitDetection).observe(owner, observer);
    }

    @MainThread
    public void observeIrregularPulseDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIrregularPulseDetection).observe(owner, observer);
    }

    @MainThread
    public void observePulseRateRangeDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mPulseRateRangeDetection).observe(owner, observer);
    }

    @MainThread
    public void observeMeasurementPositionDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mMeasurementPositionDetection).observe(owner, observer);
    }

    @MainThread
    public void observeMultipleBond(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mMultipleBond).observe(owner, observer);
    }

    @MainThread
    public void observeResponseCode(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner, observer);
    }

    @MainThread
    public void observeResponseCodeErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseCode).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseCodeErrorString(s)));
    }

    @MainThread
    public void observeResponseDelay(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner, observer);
    }

    @MainThread
    public void observeResponseDelayErrorString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mResponseDelay).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getResponseDelayErrorString(s)));
    }

    @MainThread
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<Intent> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateIsErrorResponse(boolean checked) {
        mIsErrorResponse.setValue(checked);
    }

    @MainThread
    public void updateBodyMovementDetection(boolean checked) {
        mBodyMovementDetection.setValue(checked);
    }

    @MainThread
    public void updateCuffFitDetection(boolean checked) {
        mCuffFitDetection.setValue(checked);
    }

    @MainThread
    public void updateIrregularPulseDetection(boolean checked) {
        mIrregularPulseDetection.setValue(checked);
    }

    @MainThread
    public void updatePulseRateRangeDetection(boolean checked) {
        mPulseRateRangeDetection.setValue(checked);
    }

    @MainThread
    public void updateMeasurementPositionDetection(boolean checked) {
        mMeasurementPositionDetection.setValue(checked);
    }

    @MainThread
    public void updateMultipleBond(boolean checked) {
        mMultipleBond.setValue(checked);
    }

    @MainThread
    public void updateResponseCode(@NonNull String text) {
        mResponseCode.setValue(text);
    }

    @MainThread
    public void updateResponseDelay(@NonNull String text) {
        mResponseDelay.setValue(text);
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                boolean isErrorResponse = Boolean.TRUE.equals(mIsErrorResponse.getValue());
                String responseCode = mResponseCode.getValue();
                String responseDelay = mResponseDelay.getValue();
                boolean bodyMovementDetection = Boolean.TRUE.equals(mBodyMovementDetection.getValue());
                boolean cuffFitDetection = Boolean.TRUE.equals(mCuffFitDetection.getValue());
                boolean irregularPulseDetection = Boolean.TRUE.equals(mIrregularPulseDetection.getValue());
                boolean pulseRateRangeDetection = Boolean.TRUE.equals(mPulseRateRangeDetection.getValue());
                boolean measurementPositionDetection = Boolean.TRUE.equals(mMeasurementPositionDetection.getValue());
                boolean multipleBond = Boolean.TRUE.equals(mMultipleBond.getValue());

                if (responseDelay != null && mDeviceSettingRepository.getResponseDelayErrorString(responseDelay) == null) {
                    mCharacteristicData.delay = Long.parseLong(responseDelay);
                    if (isErrorResponse) {
                        if (responseCode != null && mDeviceSettingRepository.getResponseCodeErrorString(responseCode) == null) {
                            mCharacteristicData.data = null;
                            mCharacteristicData.responseCode = Integer.parseInt(responseCode);

                            Intent intent = new Intent();
                            intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));

                            mSavedData.postValue(intent);
                            mCharacteristicData = null;
                            emitter.onComplete();
                        } else {
                            emitter.onError(new RuntimeException("Validation failed"));
                        }
                    } else {
                        mCharacteristicData.data = new BloodPressureFeature(bodyMovementDetection
                                , cuffFitDetection
                                , irregularPulseDetection
                                , pulseRateRangeDetection
                                , measurementPositionDetection
                                , multipleBond
                                , false
                                , false
                                , false).getBytes();
                        mCharacteristicData.responseCode = BluetoothGatt.GATT_SUCCESS;

                        Intent intent = new Intent();
                        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));

                        mSavedData.postValue(intent);
                        mCharacteristicData = null;
                        emitter.onComplete();
                    }
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> {
                }, onError));
    }

}