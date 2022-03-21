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

import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseServiceSettingViewModel;
import org.im97mori.ble.android.peripheral.utils.ExistObserver;
import org.im97mori.ble.android.peripheral.utils.IsNotEmptyObserver;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;

import java.util.LinkedList;
import java.util.Optional;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class BloodPressureServiceSettingViewModel extends BaseServiceSettingViewModel {

    private static final String KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED = "KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED";

    private static final String KEY_BLOOD_PRESSURE_MEASUREMENT_DATA = "KEY_BLOOD_PRESSURE_MEASUREMENT_DATA";
    private static final String KEY_INTERMEDIATE_CUFF_PRESSURE_DATA = "KEY_INTERMEDIATE_CUFF_PRESSURE_DATA";
    private static final String KEY_BLOOD_PRESSURE_FEATURE_DATA = "KEY_BLOOD_PRESSURE_FEATURE_DATA";

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

    private final MutableLiveData<Boolean> mIsIntermediateCuffPressureSupported;

    private final MutableLiveData<byte[]> mBloodPressureMeasurementData;
    private final MutableLiveData<byte[]> mIntermediateCuffPressureData;
    private final MutableLiveData<byte[]> mBloodPressureFeatureData;

    private final MutableLiveData<String> mBloodPressureMeasurementFlags;
    private final MutableLiveData<String> mBloodPressureMeasurementSystolic;
    private final MutableLiveData<String> mBloodPressureMeasurementDiastolic;
    private final MutableLiveData<String> mBloodPressureMeasurementMeanArterialPressure;
    private final MutableLiveData<String> mBloodPressureMeasurementTimeStamp;
    private final MutableLiveData<String> mBloodPressureMeasurementPulseRate;
    private final MutableLiveData<String> mBloodPressureMeasurementUserId;
    private final MutableLiveData<String> mBloodPressureMeasurementMeasurementStatus;

    private final MutableLiveData<String> mIntermediateCuffPressureFlags;
    private final MutableLiveData<String> mIntermediateCuffPressureCurrentCuffPressure;
    private final MutableLiveData<String> mIntermediateCuffPressureTimeStamp;
    private final MutableLiveData<String> mIntermediateCuffPressurePulseRate;
    private final MutableLiveData<String> mIntermediateCuffPressureUserId;
    private final MutableLiveData<String> mIntermediateCuffPressureMeasurementStatus;

    private final MutableLiveData<String> mBloodPressureFeature;

    private final MutableLiveData<Intent> mSavedData;

    @Inject
    public BloodPressureServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);

        mIsIntermediateCuffPressureSupported = savedStateHandle.getLiveData(KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED);

        mBloodPressureMeasurementData = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DATA);
        mIntermediateCuffPressureData = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_DATA);
        mBloodPressureFeatureData = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_FEATURE_DATA);

        mBloodPressureMeasurementFlags = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS);
        mBloodPressureMeasurementSystolic = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC);
        mBloodPressureMeasurementDiastolic = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC);
        mBloodPressureMeasurementMeanArterialPressure = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE);
        mBloodPressureMeasurementTimeStamp = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP);
        mBloodPressureMeasurementPulseRate = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE);
        mBloodPressureMeasurementUserId = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID);
        mBloodPressureMeasurementMeasurementStatus = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS);

        mIntermediateCuffPressureFlags = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS);
        mIntermediateCuffPressureCurrentCuffPressure = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE);
        mIntermediateCuffPressureTimeStamp = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP);
        mIntermediateCuffPressurePulseRate = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE);
        mIntermediateCuffPressureUserId = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID);
        mIntermediateCuffPressureMeasurementStatus = savedStateHandle.getLiveData(KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS);

        mBloodPressureFeature = savedStateHandle.getLiveData(KEY_BLOOD_PRESSURE_FEATURE);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mServiceData == null) {
                mServiceData = Utils.byteToParcelable(intent.getByteArrayExtra(BLOOD_PRESSURE_SERVICE.toString()), ServiceData.CREATOR);

                if (mServiceData == null) {
                    mServiceData = new ServiceData(BLOOD_PRESSURE_SERVICE
                            , BluetoothGattService.SERVICE_TYPE_PRIMARY
                            , new LinkedList<>());
                }
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

            BloodPressureMeasurement bloodPressureMeasurement;
            if (bloodPressureMeasurementOptional.isPresent()) {
                CharacteristicData characteristicData = bloodPressureMeasurementOptional.get();
                mBloodPressureMeasurementData.postValue(Utils.parcelableToByteArray(characteristicData));
                if (characteristicData.data == null) {
                    bloodPressureMeasurement = null;
                } else {
                    bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
                }
            } else {
                bloodPressureMeasurement = null;
            }

            if (mBloodPressureMeasurementFlags.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementFlags.postValue("");
                } else {
                    mBloodPressureMeasurementFlags.postValue(mDeviceSettingRepository.getHexString(bloodPressureMeasurement.getFlags(), 2));
                }
            }

            if (mBloodPressureMeasurementSystolic.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementSystolic.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                        mBloodPressureMeasurementSystolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat()));
                    } else {
                        mBloodPressureMeasurementSystolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicKpa().getSfloat()));
                    }
                }
            }

            if (mBloodPressureMeasurementDiastolic.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementDiastolic.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                        mBloodPressureMeasurementDiastolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                    } else {
                        mBloodPressureMeasurementDiastolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                    }
                }
            }

            if (mBloodPressureMeasurementMeanArterialPressure.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementMeanArterialPressure.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                        mBloodPressureMeasurementMeanArterialPressure.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat()));
                    } else {
                        mBloodPressureMeasurementMeanArterialPressure.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat()));
                    }
                }
            }

            if (mBloodPressureMeasurementTimeStamp.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementTimeStamp.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(bloodPressureMeasurement.getFlags())) {
                        mBloodPressureMeasurementTimeStamp.postValue(mDeviceSettingRepository.getDateTimeString(bloodPressureMeasurement.getYear()
                                , bloodPressureMeasurement.getMonth()
                                , bloodPressureMeasurement.getDay()
                                , bloodPressureMeasurement.getHours()
                                , bloodPressureMeasurement.getMinutes()
                                , bloodPressureMeasurement.getSeconds()));
                    } else {
                        mBloodPressureMeasurementTimeStamp.postValue("");
                    }
                }
            }

            if (mBloodPressureMeasurementPulseRate.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementPulseRate.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(bloodPressureMeasurement.getFlags())) {
                        mBloodPressureMeasurementPulseRate.postValue(String.valueOf(bloodPressureMeasurement.getPulseRate().getSfloat()));
                    } else {
                        mBloodPressureMeasurementPulseRate.postValue("");
                    }
                }
            }

            if (mBloodPressureMeasurementUserId.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementUserId.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(bloodPressureMeasurement.getFlags())) {
                        mBloodPressureMeasurementUserId.postValue(String.valueOf(bloodPressureMeasurement.getUserId()));
                    } else {
                        mBloodPressureMeasurementUserId.postValue("");
                    }
                }
            }

            if (mBloodPressureMeasurementMeasurementStatus.getValue() == null) {
                if (bloodPressureMeasurement == null) {
                    mBloodPressureMeasurementMeasurementStatus.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(bloodPressureMeasurement.getFlags())) {
                        mBloodPressureMeasurementMeasurementStatus.postValue(mDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0), 4));
                    } else {
                        mBloodPressureMeasurementMeasurementStatus.postValue("");
                    }
                }
            }

            IntermediateCuffPressure intermediateCuffPressure;
            if (intermediateCuffPressureOptional.isPresent()) {
                CharacteristicData characteristicData = intermediateCuffPressureOptional.get();
                mIntermediateCuffPressureData.postValue(Utils.parcelableToByteArray(characteristicData));
                if (characteristicData.data == null) {
                    intermediateCuffPressure = null;
                } else {
                    intermediateCuffPressure = new IntermediateCuffPressure(characteristicData.data);
                }
            } else {
                intermediateCuffPressure = null;
            }

            if (mIsIntermediateCuffPressureSupported.getValue() == null) {
                mIsIntermediateCuffPressureSupported.postValue(intermediateCuffPressureOptional.isPresent());
            }

            if (mIntermediateCuffPressureFlags.getValue() == null) {
                if (intermediateCuffPressure == null) {
                    mIntermediateCuffPressureFlags.postValue("");
                } else {
                    mIntermediateCuffPressureFlags.postValue(mDeviceSettingRepository.getHexString(intermediateCuffPressure.getFlags(), 2));
                }
            }

            if (mIntermediateCuffPressureCurrentCuffPressure.getValue() == null) {
                if (intermediateCuffPressure == null) {
                    mIntermediateCuffPressureCurrentCuffPressure.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())) {
                        mIntermediateCuffPressureCurrentCuffPressure.postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat()));
                    } else {
                        mIntermediateCuffPressureCurrentCuffPressure.postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat()));
                    }
                }
            }

            if (mIntermediateCuffPressureTimeStamp.getValue() == null) {
                if (intermediateCuffPressure == null) {
                    mIntermediateCuffPressureTimeStamp.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                        mIntermediateCuffPressureTimeStamp.postValue(mDeviceSettingRepository.getDateTimeString(intermediateCuffPressure.getYear()
                                , intermediateCuffPressure.getMonth()
                                , intermediateCuffPressure.getDay()
                                , intermediateCuffPressure.getHours()
                                , intermediateCuffPressure.getMinutes()
                                , intermediateCuffPressure.getSeconds()));
                    } else {
                        mIntermediateCuffPressureTimeStamp.postValue("");
                    }
                }
            }

            if (mIntermediateCuffPressurePulseRate.getValue() == null) {
                if (intermediateCuffPressure == null) {
                    mIntermediateCuffPressurePulseRate.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags())) {
                        mIntermediateCuffPressurePulseRate.postValue(String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat()));
                    } else {
                        mIntermediateCuffPressurePulseRate.postValue("");
                    }
                }
            }

            if (mIntermediateCuffPressureUserId.getValue() == null) {
                if (intermediateCuffPressure == null) {
                    mIntermediateCuffPressureUserId.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags())) {
                        mIntermediateCuffPressureUserId.postValue(String.valueOf(intermediateCuffPressure.getUserId()));
                    } else {
                        mIntermediateCuffPressureUserId.postValue("");
                    }
                }
            }

            if (mIntermediateCuffPressureMeasurementStatus.getValue() == null) {
                if (intermediateCuffPressure == null) {
                    mIntermediateCuffPressureMeasurementStatus.postValue("");
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                        mIntermediateCuffPressureMeasurementStatus.postValue(mDeviceSettingRepository.getHexString(BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0), 4));
                    } else {
                        mIntermediateCuffPressureMeasurementStatus.postValue("");
                    }
                }
            }

            BloodPressureFeature bloodPressureFeature;
            if (bloodPressureFeatureOptional.isPresent()) {
                CharacteristicData characteristicData = bloodPressureFeatureOptional.get();
                mBloodPressureFeatureData.postValue(Utils.parcelableToByteArray(characteristicData));
                if (characteristicData.data == null) {
                    bloodPressureFeature = null;
                } else {
                    bloodPressureFeature = new BloodPressureFeature(characteristicData.data);
                }
            } else {
                bloodPressureFeature = null;
            }

            if (mBloodPressureFeature.getValue() == null) {
                if (bloodPressureFeature == null) {
                    mBloodPressureFeature.postValue("");
                } else {
                    mBloodPressureFeature.postValue(mDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureFeature.getBloodPressureFeature(), 0), 4));
                }
            }

            emitter.onComplete();
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeHasBloodPressureMeasurementData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementData).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeIsIntermediateCuffPressureSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsIntermediateCuffPressureSupported).observe(owner, observer);
    }

    @MainThread
    public void observeHasIntermediateCuffPressureData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureData).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeHasBloodPressureFeatureData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureFeatureData).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementFlags(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementFlags).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureMeasurementSystolic(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementSystolic).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureMeasurementDiastolic(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementDiastolic).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureMeanArterialPressure(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementMeanArterialPressure).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasurementTimeStampSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementTimeStamp).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementTimeStamp).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasurementPulseRateSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementPulseRate).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementPulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementPulseRate).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasurementUserIdSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementUserId).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementUserId(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementUserId).observe(owner, observer);
    }

    @MainThread
    public void observeIsBloodPressureMeasurementMeasurementStatusSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementMeasurementStatus).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeBloodPressureMeasurementMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementMeasurementStatus).observe(owner, observer);
    }

    @MainThread
    public void observeIntermediateCuffPressureFlags(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureFlags).observe(owner, observer);
    }

    @MainThread
    public void observeIntermediateCuffPressureCurrentCuffPressure(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureCurrentCuffPressure).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressureTimeStampSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureTimeStamp).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressureTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureTimeStamp).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressurePulseRateSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressurePulseRate).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressurePulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressurePulseRate).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressureUserIdSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureUserId).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressureUserId(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureUserId).observe(owner, observer);
    }

    @MainThread
    public void observeIsIntermediateCuffPressureMeasurementStatusSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureMeasurementStatus).observe(owner, new IsNotEmptyObserver(observer));
    }

    @MainThread
    public void observeIntermediateCuffPressureMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureMeasurementStatus).observe(owner, observer);
    }

    @MainThread
    public void observeBloodPressureFeature(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureFeature).observe(owner, observer);
    }

    @MainThread
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<Intent> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateIsIntermediateCuffPressureSupported(boolean checked) {
        mIsIntermediateCuffPressureSupported.setValue(checked);
    }

    @Nullable
    @MainThread
    public byte[] getBloodPressureMeasurementData() {
        return mBloodPressureMeasurementData.getValue();
    }

    @MainThread
    public void setBloodPressureMeasurementData(@Nullable byte[] bloodPressureMeasurementData) {
        mDisposable.add(Completable.create(emitter -> {
            mBloodPressureMeasurementData.postValue(bloodPressureMeasurementData);
            if (bloodPressureMeasurementData == null) {
                mBloodPressureMeasurementFlags.postValue("");
                mBloodPressureMeasurementSystolic.postValue("");
                mBloodPressureMeasurementDiastolic.postValue("");
                mBloodPressureMeasurementMeanArterialPressure.postValue("");
                mBloodPressureMeasurementTimeStamp.postValue("");
                mBloodPressureMeasurementPulseRate.postValue("");
                mBloodPressureMeasurementUserId.postValue("");
                mBloodPressureMeasurementMeasurementStatus.postValue("");
            } else {
                CharacteristicData characteristicData = Utils.byteToParcelable(bloodPressureMeasurementData, CharacteristicData.CREATOR);
                if (characteristicData != null) {
                    if (characteristicData.data == null) {
                        mBloodPressureMeasurementFlags.postValue("");
                        mBloodPressureMeasurementSystolic.postValue("");
                        mBloodPressureMeasurementDiastolic.postValue("");
                        mBloodPressureMeasurementMeanArterialPressure.postValue("");
                        mBloodPressureMeasurementTimeStamp.postValue("");
                        mBloodPressureMeasurementPulseRate.postValue("");
                        mBloodPressureMeasurementUserId.postValue("");
                        mBloodPressureMeasurementMeasurementStatus.postValue("");
                    } else {
                        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
                        mBloodPressureMeasurementFlags.postValue(mDeviceSettingRepository.getHexString(bloodPressureMeasurement.getFlags(), 2));
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                            mBloodPressureMeasurementSystolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat()));
                        } else {
                            mBloodPressureMeasurementSystolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                            mBloodPressureMeasurementDiastolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                        } else {
                            mBloodPressureMeasurementDiastolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                            mBloodPressureMeasurementMeanArterialPressure.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat()));
                        } else {
                            mBloodPressureMeasurementMeanArterialPressure.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(bloodPressureMeasurement.getFlags())) {
                            mBloodPressureMeasurementTimeStamp.postValue(mDeviceSettingRepository.getDateTimeString(bloodPressureMeasurement.getYear()
                                    , bloodPressureMeasurement.getMonth()
                                    , bloodPressureMeasurement.getDay()
                                    , bloodPressureMeasurement.getHours()
                                    , bloodPressureMeasurement.getMinutes()
                                    , bloodPressureMeasurement.getSeconds()));
                        } else {
                            mBloodPressureMeasurementTimeStamp.postValue("");
                        }
                        if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(bloodPressureMeasurement.getFlags())) {
                            mBloodPressureMeasurementPulseRate.postValue(String.valueOf(bloodPressureMeasurement.getPulseRate().getSfloat()));
                        } else {
                            mBloodPressureMeasurementPulseRate.postValue("");
                        }
                        if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(bloodPressureMeasurement.getFlags())) {
                            mBloodPressureMeasurementUserId.postValue(String.valueOf(bloodPressureMeasurement.getUserId()));
                        } else {
                            mBloodPressureMeasurementUserId.postValue("");
                        }
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(bloodPressureMeasurement.getFlags())) {
                            mBloodPressureMeasurementMeasurementStatus
                                    .postValue(mDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0), 4));
                        } else {
                            mBloodPressureMeasurementMeasurementStatus.postValue("");
                        }
                    }
                }
            }
            emitter.onComplete();
        }).subscribeOn(Schedulers.io())
                .subscribe());
    }

    @Nullable
    @MainThread
    public byte[] getIntermediateCuffPressureData() {
        return mIntermediateCuffPressureData.getValue();
    }

    @MainThread
    public void setIntermediateCuffPressureData(@Nullable byte[] intermediateCuffPressureData) {
        mDisposable.add(Completable.create(emitter -> {
            mIntermediateCuffPressureData.postValue(intermediateCuffPressureData);
            if (intermediateCuffPressureData == null) {
                mIntermediateCuffPressureFlags.postValue("");
                mIntermediateCuffPressureCurrentCuffPressure.postValue("");
                mIntermediateCuffPressureTimeStamp.postValue("");
                mIntermediateCuffPressurePulseRate.postValue("");
                mIntermediateCuffPressureUserId.postValue("");
                mIntermediateCuffPressureMeasurementStatus.postValue("");
            } else {
                CharacteristicData characteristicData = Utils.byteToParcelable(intermediateCuffPressureData, CharacteristicData.CREATOR);
                if (characteristicData != null) {
                    if (characteristicData.data == null) {
                        mIntermediateCuffPressureFlags.postValue("");
                        mIntermediateCuffPressureCurrentCuffPressure.postValue("");
                        mIntermediateCuffPressureTimeStamp.postValue("");
                        mIntermediateCuffPressurePulseRate.postValue("");
                        mIntermediateCuffPressureUserId.postValue("");
                        mIntermediateCuffPressureMeasurementStatus.postValue("");
                    } else {
                        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(characteristicData.data);
                        mIntermediateCuffPressureFlags.postValue(mDeviceSettingRepository.getHexString(intermediateCuffPressure.getFlags(), 2));
                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())) {
                            mIntermediateCuffPressureCurrentCuffPressure
                                    .postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat()));
                        } else {
                            mIntermediateCuffPressureCurrentCuffPressure
                                    .postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat()));
                        }
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            mIntermediateCuffPressureTimeStamp.postValue(mDeviceSettingRepository.getDateTimeString(intermediateCuffPressure.getYear()
                                    , intermediateCuffPressure.getMonth()
                                    , intermediateCuffPressure.getDay()
                                    , intermediateCuffPressure.getHours()
                                    , intermediateCuffPressure.getMinutes()
                                    , intermediateCuffPressure.getSeconds()));
                        } else {
                            mIntermediateCuffPressureTimeStamp.postValue("");
                        }
                        if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags())) {
                            mIntermediateCuffPressurePulseRate.postValue(String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat()));
                        } else {
                            mIntermediateCuffPressurePulseRate.postValue("");
                        }
                        if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags())) {
                            mIntermediateCuffPressureUserId.postValue(String.valueOf(intermediateCuffPressure.getUserId()));
                        } else {
                            mIntermediateCuffPressureUserId.postValue("");
                        }
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                            mIntermediateCuffPressureMeasurementStatus
                                    .postValue(mDeviceSettingRepository.getHexString(BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0), 4));
                        } else {
                            mIntermediateCuffPressureMeasurementStatus.postValue("");
                        }
                    }
                }
            }
            emitter.onComplete();
        }).subscribeOn(Schedulers.io())
                .subscribe());
    }

    @Nullable
    @MainThread
    public byte[] getBloodPressureFeatureData() {
        return mBloodPressureFeatureData.getValue();
    }

    @MainThread
    public void setBloodPressureFeatureData(@Nullable byte[] bloodPressureFeatureData) {
        mDisposable.add(Completable.create(emitter -> {
            mBloodPressureFeatureData.postValue(bloodPressureFeatureData);
            if (bloodPressureFeatureData == null) {
                mBloodPressureFeature.postValue("");
            } else {
                CharacteristicData characteristicData = Utils.byteToParcelable(bloodPressureFeatureData, CharacteristicData.CREATOR);
                if (characteristicData != null) {
                    if (characteristicData.data == null) {
                        mBloodPressureFeature.postValue("");
                    } else {
                        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(characteristicData.data);
                        mBloodPressureFeature
                                .postValue(mDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureFeature.getBloodPressureFeature(), 0), 4));
                    }
                }
            }
            emitter.onComplete();
        }).subscribeOn(Schedulers.io())
                .subscribe());
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mServiceData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                mServiceData.characteristicDataList.clear();

                byte[] bloodPressureMeasurementData = mBloodPressureMeasurementData.getValue();
                if (bloodPressureMeasurementData != null) {
                    mServiceData.characteristicDataList.add(Utils.byteToParcelable(bloodPressureMeasurementData, CharacteristicData.CREATOR));
                }

                if (Boolean.TRUE.equals(mIsIntermediateCuffPressureSupported.getValue())) {
                    byte[] intermediateCuffPressureData = mIntermediateCuffPressureData.getValue();
                    if (intermediateCuffPressureData != null) {
                        mServiceData.characteristicDataList.add(Utils.byteToParcelable(intermediateCuffPressureData, CharacteristicData.CREATOR));
                    }
                }

                byte[] bloodPressureFeature = mBloodPressureFeatureData.getValue();
                if (bloodPressureFeature != null) {
                    mServiceData.characteristicDataList.add(Utils.byteToParcelable(bloodPressureFeature, CharacteristicData.CREATOR));
                }

                if (mServiceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> !characteristicData.uuid.equals(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC)).count() == 2) {
                    Intent intent = new Intent();
                    intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), Utils.parcelableToByteArray(mServiceData));

                    mSavedData.postValue(intent);
                    mServiceData = null;
                    emitter.onComplete();
                } else {
                    emitter.onError(new RuntimeException("No data"));
                }

            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> {
                }, onError));
    }
}