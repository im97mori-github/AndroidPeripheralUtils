package org.im97mori.ble.android.peripheral.ui.device.setting.u1810;

import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;

import android.bluetooth.BluetoothGattService;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseServiceSettingViewModel;
import org.im97mori.ble.android.peripheral.utils.ExistObserver;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;

import java.util.Optional;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class BloodPressureServiceSettingViewModel extends BaseServiceSettingViewModel {

    private static final String KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED = "KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED";

    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON = "KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON";
    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON = "KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON";
    private static final String KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON = "KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON";

    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS = "KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS";
    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC = "KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC";
    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC = "KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC";
    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE = "KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE";
    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP = "KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP";
    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE = "KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE";
    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID = "KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID";
    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS = "KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS";

    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS = "KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS";
    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE = "KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE";
    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP = "KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP";
    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE = "KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE";
    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID = "KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID";
    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS = "KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS";

    private static final String KEY_BLOOD_PRESSURE_FEATURE = "KEY_BLOOD_PRESSURE_FEATURE";

    private final SavedStateHandle mSavedStateHandle;

    private final MutableLiveData<Boolean> mIsIntermediateCuffPressureSupported;

    private final MutableLiveData<String> mBloodPressureMeasurementDataJson;
    private final MutableLiveData<String> mIntermediateCuffPressureDataJson;
    private final MutableLiveData<String> mBloodPressureFeatureDataJson;

    @Inject
    public BloodPressureServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceRepository deviceRepository, @NonNull Gson gson) {
        super(deviceRepository, gson);
        mSavedStateHandle = savedStateHandle;

        mIsIntermediateCuffPressureSupported = savedStateHandle.getLiveData(KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED);

        mBloodPressureMeasurementDataJson = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON);
        mIntermediateCuffPressureDataJson = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON);
        mBloodPressureFeatureDataJson = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON);

    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        return Completable.create(emitter -> {
            if (mServiceData == null) {
                String dataJson = intent.getStringExtra(BLOOD_PRESSURE_SERVICE.toString());
                try {
                    mServiceData = mGson.fromJson(dataJson, ServiceData.class);
                } catch (JsonSyntaxException e) {
                    e.printStackTrace();
                }

                if (mServiceData == null) {
                    mServiceData = new ServiceData();
                    mServiceData.uuid = BLOOD_PRESSURE_SERVICE;
                    mServiceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;
                }

                Optional<CharacteristicData> bloodPressureMeasurementOptional = mServiceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> characteristicData.uuid.equals(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC))
                        .findAny();

                Optional<CharacteristicData> intermediateCuffPressureOptional = mServiceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> characteristicData.uuid.equals(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC))
                        .findAny();

                Optional<CharacteristicData> bloodPressureFeatureOptional = mServiceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> characteristicData.uuid.equals(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC))
                        .findAny();

                if (mBloodPressureMeasurementDataJson.getValue() == null) {
                    if (bloodPressureMeasurementOptional.isPresent()) {
                        CharacteristicData characteristicData = bloodPressureMeasurementOptional.get();
                        mBloodPressureMeasurementDataJson.postValue(mGson.toJson(characteristicData));
                        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
                        mSavedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS)
                                .postValue(Integer.toHexString(bloodPressureMeasurement.getFlags()));
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC)
                                    .postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC)
                                    .postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC)
                                    .postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC)
                                    .postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE)
                                    .postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE)
                                    .postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(bloodPressureMeasurement.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP).postValue(mDeviceRepository.getDateTimeString(bloodPressureMeasurement.getYear()
                                    , bloodPressureMeasurement.getMonth()
                                    , bloodPressureMeasurement.getDay()
                                    , bloodPressureMeasurement.getHours()
                                    , bloodPressureMeasurement.getMinutes()
                                    , bloodPressureMeasurement.getSeconds()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP).postValue(null);
                        }
                        if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(bloodPressureMeasurement.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE).postValue(String.valueOf(bloodPressureMeasurement.getPulseRate().getSfloat()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE).postValue(null);
                        }
                        if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(bloodPressureMeasurement.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID).postValue(String.valueOf(bloodPressureMeasurement.getUserId()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID).postValue(null);
                        }
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(bloodPressureMeasurement.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS).
                                    postValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0)));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS).postValue(null);
                        }
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP).postValue(null);
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE).postValue(null);
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID).postValue(null);
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS).postValue(null);
                    }
                }

                if (mIsIntermediateCuffPressureSupported.getValue() == null) {
                    mIsIntermediateCuffPressureSupported.postValue(intermediateCuffPressureOptional.isPresent());
                }

                if (mIntermediateCuffPressureDataJson.getValue() == null) {
                    if (intermediateCuffPressureOptional.isPresent()) {
                        CharacteristicData characteristicData = intermediateCuffPressureOptional.get();
                        mIntermediateCuffPressureDataJson.postValue(mGson.toJson(characteristicData));
                        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(characteristicData.data);
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS)
                                .postValue(Integer.toHexString(intermediateCuffPressure.getFlags()));
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE)
                                    .postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE)
                                    .postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP)
                                    .postValue(mDeviceRepository.getDateTimeString(intermediateCuffPressure.getYear()
                                            , intermediateCuffPressure.getMonth()
                                            , intermediateCuffPressure.getDay()
                                            , intermediateCuffPressure.getHours()
                                            , intermediateCuffPressure.getMinutes()
                                            , intermediateCuffPressure.getSeconds()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP).postValue(null);
                        }
                        if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE).postValue(String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE).postValue(null);
                        }
                        if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID).postValue(String.valueOf(intermediateCuffPressure.getUserId()));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID).postValue(null);
                        }
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS)
                                    .postValue(Integer.toHexString(BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0)));
                        } else {
                            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS).postValue(null);
                        }
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP).postValue(null);
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE).postValue(null);
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID).postValue(null);
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS).postValue(null);
                    }
                }

                if (mBloodPressureFeatureDataJson.getValue() == null) {
                    if (bloodPressureFeatureOptional.isPresent()) {
                        CharacteristicData characteristicData = bloodPressureFeatureOptional.get();
                        mBloodPressureFeatureDataJson.postValue(mGson.toJson(characteristicData));
                        if (characteristicData.data != null) {
                            BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(characteristicData.data);
                            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_FEATURE).postValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureFeature.getBloodPressureFeature(), 0)));
                        }
                    }
                }

                emitter.onComplete();
            } else {
                emitter.onError(new RuntimeException("Initialized"));
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }

    @MainThread
    public void observeHasBloodPressureMeasurementDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementDataJson).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeHasIntermediateCuffPressureDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureDataJson).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeHasBloodPressureFeatureDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureFeatureDataJson).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeIsIntermediateCuffPressureSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsIntermediateCuffPressureSupported).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureMeasurementFlags(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS)).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureMeasurementSystolic(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC)).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureMeasurementDiastolic(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC)).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureMeanArterialPressure(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE)).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasurementTimeStampSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP)).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasurementPulseRateSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementPulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE)).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasurementUserIdSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementUserId(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID)).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasuremenMeasurementStatusSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS)).observe(owner, observer);
    }


    @MainThread
    public void observeIntermediateCuffPressureFlags(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS)).observe(owner, observer);
    }

    @MainThread
    public void observeIntermediateCuffPressureCurrentCuffPressure(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE)).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressureTimeStampSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressureTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP)).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressurePulseRateSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressurePulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE)).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressureUserIdSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressureUserId(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID)).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressureMeasurementStatusSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS)).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressureMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS)).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureFeature(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_FEATURE)).observe(owner, observer);
    }

    @MainThread
    public void updateIsIntermediateCuffPressureSupported(boolean checked) {
        mIsIntermediateCuffPressureSupported.setValue(checked);
    }

    @Nullable
    @MainThread
    public String getBloodPressureMeasurementDataJson() {
        return mBloodPressureMeasurementDataJson.getValue();
    }

    @MainThread
    public void setBloodPressureMeasurementDataJson(@Nullable String bloodPressureMeasurementDataJson) {
        mBloodPressureMeasurementDataJson.setValue(bloodPressureMeasurementDataJson);
        if (bloodPressureMeasurementDataJson == null) {
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS).setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(bloodPressureMeasurementDataJson, CharacteristicData.class);
                if (characteristicData.data == null) {
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS).setValue(null);
                } else {
                    BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS).setValue(Integer.toHexString(bloodPressureMeasurement.getFlags()));
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC)
                                .setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC)
                                .setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                    }
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC)
                                .setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC)
                                .setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                    }
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE)
                                .setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE)
                                .setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat()));
                    }
                    if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(bloodPressureMeasurement.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP).setValue(mDeviceRepository.getDateTimeString(bloodPressureMeasurement.getYear()
                                , bloodPressureMeasurement.getMonth()
                                , bloodPressureMeasurement.getDay()
                                , bloodPressureMeasurement.getHours()
                                , bloodPressureMeasurement.getMinutes()
                                , bloodPressureMeasurement.getSeconds()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP).setValue(null);
                    }
                    if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(bloodPressureMeasurement.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE).setValue(String.valueOf(bloodPressureMeasurement.getPulseRate().getSfloat()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE).setValue(null);
                    }
                    if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(bloodPressureMeasurement.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID).setValue(String.valueOf(bloodPressureMeasurement.getUserId()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID).setValue(null);
                    }
                    if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(bloodPressureMeasurement.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS)
                                .setValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0)));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS).setValue(null);
                    }
                }
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    @MainThread
    public String getIntermediateCuffPressureDataJson() {
        return mIntermediateCuffPressureDataJson.getValue();
    }

    @MainThread
    public void setIntermediateCuffPressureDataJson(@Nullable String intermediateCuffPressureDataJson) {
        mIntermediateCuffPressureDataJson.setValue(intermediateCuffPressureDataJson);
        if (intermediateCuffPressureDataJson == null) {
            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID).setValue(null);
            mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS).setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(intermediateCuffPressureDataJson, CharacteristicData.class);
                if (characteristicData.data == null) {
                    mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID).setValue(null);
                    mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS).setValue(null);
                } else {
                    IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(characteristicData.data);
                    mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS).setValue(Integer.toHexString(intermediateCuffPressure.getFlags()));
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE)
                                .setValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE)
                                .setValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat()));
                    }
                    if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP).setValue(mDeviceRepository.getDateTimeString(intermediateCuffPressure.getYear()
                                , intermediateCuffPressure.getMonth()
                                , intermediateCuffPressure.getDay()
                                , intermediateCuffPressure.getHours()
                                , intermediateCuffPressure.getMinutes()
                                , intermediateCuffPressure.getSeconds()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP).setValue(null);
                    }
                    if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE).setValue(String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE).setValue(null);
                    }
                    if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID).setValue(String.valueOf(intermediateCuffPressure.getUserId()));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID).setValue(null);
                    }
                    if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS)
                                .setValue(Integer.toHexString(BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0)));
                    } else {
                        mSavedStateHandle.<String>getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS).setValue(null);
                    }
                }
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    @MainThread
    public String getBloodPressureFeatureDataJson() {
        return mBloodPressureFeatureDataJson.getValue();
    }

    @MainThread
    public void setBloodPressureFeatureDataJson(@Nullable String bloodPressureFeatureDataJson) {
        mBloodPressureFeatureDataJson.setValue(bloodPressureFeatureDataJson);
        if (bloodPressureFeatureDataJson == null) {
            mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_FEATURE).setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(bloodPressureFeatureDataJson, CharacteristicData.class);
                if (characteristicData.data == null) {
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_FEATURE).setValue(null);
                } else {
                    BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(characteristicData.data);
                    mSavedStateHandle.<String>getLiveData(KEY_BLOOD_PRESSURE_FEATURE).setValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureFeature.getBloodPressureFeature(), 0)));
                }
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @NonNull
    @Override
    public Single<Intent> save() {
        return Single.<Intent>create(emitter -> {
            ServiceData serviceData = mServiceData;
            if (serviceData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                serviceData.characteristicDataList.clear();

                String bloodPressureMeasurementJson = mBloodPressureMeasurementDataJson.getValue();
                if (bloodPressureMeasurementJson != null) {
                    try {
                        serviceData.characteristicDataList.add(mGson.fromJson(bloodPressureMeasurementJson, CharacteristicData.class));
                    } catch (JsonSyntaxException e) {
                        e.printStackTrace();
                    }
                }

                if (Boolean.TRUE.equals(mIsIntermediateCuffPressureSupported.getValue())) {
                    String intermediateCuffPressureJson = mIntermediateCuffPressureDataJson.getValue();
                    if (intermediateCuffPressureJson != null) {
                        try {
                            serviceData.characteristicDataList.add(mGson.fromJson(intermediateCuffPressureJson, CharacteristicData.class));
                        } catch (JsonSyntaxException e) {
                            e.printStackTrace();
                        }
                    }
                }

                String bloodPressureFeatureJson = mBloodPressureFeatureDataJson.getValue();
                if (bloodPressureFeatureJson != null) {
                    try {
                        serviceData.characteristicDataList.add(mGson.fromJson(bloodPressureFeatureJson, CharacteristicData.class));
                    } catch (JsonSyntaxException e) {
                        e.printStackTrace();
                    }
                }

                if (serviceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> !characteristicData.uuid.equals(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC)).count() == 2) {
                    Intent intent = new Intent();
                    intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
                    mServiceData = null;
                    emitter.onSuccess(intent);
                } else {
                    emitter.onError(new RuntimeException("No data"));
                }

            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }

}