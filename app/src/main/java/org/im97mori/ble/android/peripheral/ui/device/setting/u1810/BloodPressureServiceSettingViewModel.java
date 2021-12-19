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

import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseServiceSettingViewModel;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;

import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BloodPressureServiceSettingViewModel extends BaseServiceSettingViewModel {

    private final MutableLiveData<Boolean> hasBloodPressureMeasurement;
    private final MutableLiveData<Boolean> hasIntermediateCuffPressure;
    private final MutableLiveData<Boolean> supportIntermediateCuffPressure;
    private final MutableLiveData<Boolean> hasBloodPressureFeature;

    private final MutableLiveData<String> mBloodPressureMeasurementJson;
    private final MutableLiveData<String> mIntermediateCuffPressureJson;
    private final MutableLiveData<String> mBloodPressureFeatureJson;

    private final MutableLiveData<String> mBloodPressureMeasurementFlags;
    private final MutableLiveData<String> mBloodPressureMeasurementSystolic;
    private final MutableLiveData<String> mBloodPressureMeasurementDiastolic;
    private final MutableLiveData<String> mBloodPressureMeasurementMeanArterialPressure;
    private final MutableLiveData<Boolean> hasBloodPressureMeasurementTimeStamp;
    private final MutableLiveData<String> mBloodPressureMeasurementTimeStamp;
    private final MutableLiveData<Boolean> hasBloodPressureMeasurementPulseRate;
    private final MutableLiveData<String> mBloodPressureMeasurementPulseRate;
    private final MutableLiveData<Boolean> hasBloodPressureMeasurementUserId;
    private final MutableLiveData<String> mBloodPressureMeasurementUserId;
    private final MutableLiveData<Boolean> hasBloodPressureMeasurementMeasurementStatus;
    private final MutableLiveData<String> mBloodPressureMeasurementMeasurementStatus;

    private final MutableLiveData<String> mIntermediateCuffPressureFlags;
    private final MutableLiveData<String> mIntermediateCuffPressureCurrentCuffPressure;
    private final MutableLiveData<Boolean> hasIntermediateCuffPressureTimeStamp;
    private final MutableLiveData<String> mIntermediateCuffPressureTimeStamp;
    private final MutableLiveData<Boolean> hasIntermediateCuffPressurePulseRate;
    private final MutableLiveData<String> mIntermediateCuffPressurePulseRate;
    private final MutableLiveData<Boolean> hasIntermediateCuffPressureUserId;
    private final MutableLiveData<String> mIntermediateCuffPressureUserId;
    private final MutableLiveData<Boolean> hasIntermediateCuffPressureMeasurementStatus;
    private final MutableLiveData<String> mIntermediateCuffPressureMeasurementStatus;

    private final MutableLiveData<String> mBloodPressureFeature;

    public BloodPressureServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        hasBloodPressureMeasurement = savedStateHandle.getLiveData("hasBloodPressureMeasurement");
        hasIntermediateCuffPressure = savedStateHandle.getLiveData("hasIntermediateCuffPressure");
        hasBloodPressureFeature = savedStateHandle.getLiveData("hasBloodPressureFeature");
        supportIntermediateCuffPressure = savedStateHandle.getLiveData("supportIntermediateCuffPressure");

        mBloodPressureMeasurementJson = savedStateHandle.getLiveData("mBloodPressureMeasurementJson");
        mIntermediateCuffPressureJson = savedStateHandle.getLiveData("mIntermediateCuffPressureJson");
        mBloodPressureFeatureJson = savedStateHandle.getLiveData("mBloodPressureFeatureJson");

        mBloodPressureMeasurementFlags = savedStateHandle.getLiveData("mBloodPressureMeasurementFlags");
        mBloodPressureMeasurementSystolic = savedStateHandle.getLiveData("mBloodPressureMeasurementSystolic");
        mBloodPressureMeasurementDiastolic = savedStateHandle.getLiveData("mBloodPressureMeasurementDiastolic");
        mBloodPressureMeasurementMeanArterialPressure = savedStateHandle.getLiveData("mBloodPressureMeasurementMeanArterialPressure");
        hasBloodPressureMeasurementTimeStamp = savedStateHandle.getLiveData("hasBloodPressureMeasurementTimeStamp");
        mBloodPressureMeasurementTimeStamp = savedStateHandle.getLiveData("mBloodPressureMeasurementTimeStamp");
        hasBloodPressureMeasurementPulseRate = savedStateHandle.getLiveData("hasBloodPressureMeasurementPulseRate");
        mBloodPressureMeasurementPulseRate = savedStateHandle.getLiveData("mBloodPressureMeasurementPulseRate");
        hasBloodPressureMeasurementUserId = savedStateHandle.getLiveData("hasBloodPressureMeasurementUserId");
        mBloodPressureMeasurementUserId = savedStateHandle.getLiveData("mBloodPressureMeasurementUserId");
        hasBloodPressureMeasurementMeasurementStatus = savedStateHandle.getLiveData("hasBloodPressureMeasurementMeasurementStatus");
        mBloodPressureMeasurementMeasurementStatus = savedStateHandle.getLiveData("mBloodPressureMeasurementMeasurementStatus");

        mIntermediateCuffPressureFlags = savedStateHandle.getLiveData("mIntermediateCuffPressureFlags");
        mIntermediateCuffPressureCurrentCuffPressure = savedStateHandle.getLiveData("mIntermediateCuffPressureSystolic");
        hasIntermediateCuffPressureTimeStamp = savedStateHandle.getLiveData("hasIntermediateCuffPressureTimeStamp");
        mIntermediateCuffPressureTimeStamp = savedStateHandle.getLiveData("mIntermediateCuffPressureTimeStamp");
        hasIntermediateCuffPressurePulseRate = savedStateHandle.getLiveData("hasIntermediateCuffPressurePulseRate");
        mIntermediateCuffPressurePulseRate = savedStateHandle.getLiveData("mIntermediateCuffPressurePulseRate");
        hasIntermediateCuffPressureUserId = savedStateHandle.getLiveData("hasIntermediateCuffPressureUserId");
        mIntermediateCuffPressureUserId = savedStateHandle.getLiveData("mIntermediateCuffPressureUserId");
        hasIntermediateCuffPressureMeasurementStatus = savedStateHandle.getLiveData("hasIntermediateCuffPressureMeasurementStatus");
        mIntermediateCuffPressureMeasurementStatus = savedStateHandle.getLiveData("mIntermediateCuffPressureMeasurementStatus");

        mBloodPressureFeature = savedStateHandle.getLiveData("mBloodPressureFeature");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mServiceData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(BLOOD_PRESSURE_SERVICE.toString())))
                    .subscribeOn(Schedulers.io())
                    .observeOn(Schedulers.io())
                    .flatMapCompletable(dataString -> {
                        if (dataString.isPresent()) {
                            try {
                                mServiceData = mGson.fromJson(dataString.get(), ServiceData.class);
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        if (mServiceData == null) {
                            mServiceData = new ServiceData();
                            mServiceData.uuid = BLOOD_PRESSURE_SERVICE;
                            mServiceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;
                        }

                        if (hasBloodPressureMeasurement.getValue() == null) {
                            Optional<CharacteristicData> optional = mServiceData.characteristicDataList
                                    .stream()
                                    .filter(characteristicData -> BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.equals(characteristicData.uuid))
                                    .findAny();
                            hasBloodPressureMeasurement.postValue(optional.isPresent());
                            if (optional.isPresent()) {
                                CharacteristicData characteristicData = optional.get();
                                if (mBloodPressureMeasurementJson.getValue() == null) {
                                    mBloodPressureMeasurementJson.postValue(mGson.toJson(characteristicData));
                                }
                                if (characteristicData.data != null) {
                                    BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
                                    if (mBloodPressureMeasurementFlags.getValue() == null) {
                                        mBloodPressureMeasurementFlags.postValue(Integer.toHexString(bloodPressureMeasurement.getFlags()));
                                    }
                                    if (mBloodPressureMeasurementSystolic.getValue() == null) {
                                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                                            mBloodPressureMeasurementSystolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                                        } else {
                                            mBloodPressureMeasurementSystolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                                        }
                                    }
                                    if (mBloodPressureMeasurementDiastolic.getValue() == null) {
                                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                                            mBloodPressureMeasurementDiastolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                                        } else {
                                            mBloodPressureMeasurementDiastolic.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                                        }
                                    }
                                    if (mBloodPressureMeasurementMeanArterialPressure.getValue() == null) {
                                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                                            mBloodPressureMeasurementMeanArterialPressure.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat()));
                                        } else {
                                            mBloodPressureMeasurementMeanArterialPressure.postValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat()));
                                        }
                                    }
                                    if (hasBloodPressureMeasurementTimeStamp.getValue() == null) {
                                        hasBloodPressureMeasurementTimeStamp.postValue(BloodPressureMeasurementUtils.isFlagsTimeStampPresent(bloodPressureMeasurement.getFlags()));
                                    }
                                    if (mBloodPressureMeasurementTimeStamp.getValue() == null) {
                                        mBloodPressureMeasurementTimeStamp.postValue(mResourceTextSource.getDateTimeString(bloodPressureMeasurement.getYear()
                                                , bloodPressureMeasurement.getMonth()
                                                , bloodPressureMeasurement.getDay()
                                                , bloodPressureMeasurement.getHours()
                                                , bloodPressureMeasurement.getMinutes()
                                                , bloodPressureMeasurement.getSeconds()));
                                    }
                                    if (hasBloodPressureMeasurementPulseRate.getValue() == null) {
                                        hasBloodPressureMeasurementPulseRate.postValue(BloodPressureMeasurementUtils.isFlagsPulseRatePresent(bloodPressureMeasurement.getFlags()));
                                    }
                                    if (mBloodPressureMeasurementPulseRate.getValue() == null) {
                                        mBloodPressureMeasurementPulseRate.postValue(String.valueOf(bloodPressureMeasurement.getPulseRate().getSfloat()));
                                    }
                                    if (hasBloodPressureMeasurementUserId.getValue() == null) {
                                        hasBloodPressureMeasurementUserId.postValue(BloodPressureMeasurementUtils.isFlagsUserIdPresent(bloodPressureMeasurement.getFlags()));
                                    }
                                    if (mBloodPressureMeasurementUserId.getValue() == null) {
                                        mBloodPressureMeasurementUserId.postValue(String.valueOf(bloodPressureMeasurement.getUserId()));
                                    }
                                    if (hasBloodPressureMeasurementMeasurementStatus.getValue() == null) {
                                        hasBloodPressureMeasurementMeasurementStatus.postValue(BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(bloodPressureMeasurement.getFlags()));
                                    }
                                    if (mBloodPressureMeasurementMeasurementStatus.getValue() == null) {
                                        mBloodPressureMeasurementMeasurementStatus.postValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0)));
                                    }
                                }
                            }
                        }

                        if (hasIntermediateCuffPressure.getValue() == null) {
                            Optional<CharacteristicData> optional = mServiceData.characteristicDataList
                                    .stream()
                                    .filter(characteristicData -> INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.equals(characteristicData.uuid))
                                    .findAny();
                            hasIntermediateCuffPressure.postValue(optional.isPresent());
                            if (optional.isPresent()) {
                                CharacteristicData characteristicData = optional.get();
                                if (mIntermediateCuffPressureJson.getValue() == null) {
                                    mIntermediateCuffPressureJson.postValue(mGson.toJson(characteristicData));
                                }
                                if (characteristicData.data != null) {
                                    IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(characteristicData.data);
                                    if (mIntermediateCuffPressureFlags.getValue() == null) {
                                        mIntermediateCuffPressureFlags.postValue(Integer.toHexString(intermediateCuffPressure.getFlags()));
                                    }
                                    if (mIntermediateCuffPressureCurrentCuffPressure.getValue() == null) {
                                        if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())) {
                                            mIntermediateCuffPressureCurrentCuffPressure.postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat()));
                                        } else {
                                            mIntermediateCuffPressureCurrentCuffPressure.postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat()));
                                        }
                                    }
                                    if (hasIntermediateCuffPressureTimeStamp.getValue() == null) {
                                        hasIntermediateCuffPressureTimeStamp.postValue(BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags()));
                                    }
                                    if (mIntermediateCuffPressureTimeStamp.getValue() == null) {
                                        mIntermediateCuffPressureTimeStamp.postValue(mResourceTextSource.getDateTimeString(intermediateCuffPressure.getYear()
                                                , intermediateCuffPressure.getMonth()
                                                , intermediateCuffPressure.getDay()
                                                , intermediateCuffPressure.getHours()
                                                , intermediateCuffPressure.getMinutes()
                                                , intermediateCuffPressure.getSeconds()));
                                    }
                                    if (hasIntermediateCuffPressurePulseRate.getValue() == null) {
                                        hasIntermediateCuffPressurePulseRate.postValue(BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags()));
                                    }
                                    if (mIntermediateCuffPressurePulseRate.getValue() == null) {
                                        mIntermediateCuffPressurePulseRate.postValue(String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat()));
                                    }
                                    if (hasIntermediateCuffPressureUserId.getValue() == null) {
                                        hasIntermediateCuffPressureUserId.postValue(BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags()));
                                    }
                                    if (mIntermediateCuffPressureUserId.getValue() == null) {
                                        mIntermediateCuffPressureUserId.postValue(String.valueOf(intermediateCuffPressure.getUserId()));
                                    }
                                    if (hasIntermediateCuffPressureMeasurementStatus.getValue() == null) {
                                        hasIntermediateCuffPressureMeasurementStatus.postValue(BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags()));
                                    }
                                    if (mIntermediateCuffPressureMeasurementStatus.getValue() == null) {
                                        mIntermediateCuffPressureMeasurementStatus.postValue(Integer.toHexString(BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0)));
                                    }
                                }
                            }
                            if (supportIntermediateCuffPressure.getValue() == null) {
                                supportIntermediateCuffPressure.postValue(optional.isPresent());
                            }
                        }

                        if (hasBloodPressureFeature.getValue() == null) {
                            Optional<CharacteristicData> optional = mServiceData.characteristicDataList
                                    .stream()
                                    .filter(characteristicData -> BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.equals(characteristicData.uuid))
                                    .findAny();
                            hasBloodPressureFeature.postValue(optional.isPresent());
                            if (optional.isPresent()) {
                                CharacteristicData characteristicData = optional.get();
                                if (mBloodPressureFeatureJson.getValue() == null) {
                                    mBloodPressureFeatureJson.postValue(mGson.toJson(characteristicData));
                                }
                                if (characteristicData.data != null) {
                                    BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(characteristicData.data);
                                    if (mBloodPressureFeature.getValue() == null) {
                                        mBloodPressureFeature.postValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureFeature.getBloodPressureFeature(), 0)));
                                    }
                                }
                            }
                        }

                        return Completable.complete();
                    });
        } else {
            completable = Completable.complete();
        }
        return completable;
    }

    public void observeHasBloodPressureMeasurement(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasBloodPressureMeasurement).observe(owner, observer);
    }

    public void observeHasIntermediateCuffPressure(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasIntermediateCuffPressure).observe(owner, observer);
    }

    public void observeHasBloodPressureFeature(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasBloodPressureFeature).observe(owner, observer);
    }

    public void observeBloodPressureMeasurementFlags(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementFlags).observe(owner, observer);
    }

    public void observeBloodPressureMeasurementSystolic(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementSystolic).observe(owner, observer);
    }

    public void observeBloodPressureMeasurementDiastolic(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementDiastolic).observe(owner, observer);
    }

    public void observeBloodPressureMeanArterialPressure(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementMeanArterialPressure).observe(owner, observer);
    }

    public void observeHasBloodPressureMeasurementTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasBloodPressureMeasurementTimeStamp).observe(owner, observer);
    }

    public void observeBloodPressureMeasurementTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementTimeStamp).observe(owner, observer);
    }

    public void observeHasBloodPressureMeasurementPulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasBloodPressureMeasurementPulseRate).observe(owner, observer);
    }

    public void observeBloodPressureMeasurementPulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementPulseRate).observe(owner, observer);
    }

    public void observeHasBloodPressureMeasurementUserId(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasBloodPressureMeasurementUserId).observe(owner, observer);
    }

    public void observeBloodPressureMeasurementUserId(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementUserId).observe(owner, observer);
    }

    public void observeHasBloodPressureMeasuremenMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasBloodPressureMeasurementMeasurementStatus).observe(owner, observer);
    }

    public void observeBloodPressureMeasurementMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureMeasurementMeasurementStatus).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIntermediateCuffPressure(Boolean checked) {
        supportIntermediateCuffPressure.setValue(checked);
        if (!checked && Boolean.TRUE.equals(hasIntermediateCuffPressure.getValue())) {
            hasIntermediateCuffPressure.setValue(false);
        }
    }

    public void observeIntermediateCuffPressureFlags(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureFlags).observe(owner, observer);
    }

    public void observeIntermediateCuffPressureCurrentCuffPressure(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureCurrentCuffPressure).observe(owner, observer);
    }

    public void observeHasIntermediateCuffPressureTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasIntermediateCuffPressureTimeStamp).observe(owner, observer);
    }

    public void observeIntermediateCuffPressureTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureTimeStamp).observe(owner, observer);
    }

    public void observeHasIntermediateCuffPressurePulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasIntermediateCuffPressurePulseRate).observe(owner, observer);
    }

    public void observeIntermediateCuffPressurePulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressurePulseRate).observe(owner, observer);
    }

    public void observeHasIntermediateCuffPressureUserId(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasIntermediateCuffPressureUserId).observe(owner, observer);
    }

    public void observeIntermediateCuffPressureUserId(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureUserId).observe(owner, observer);
    }

    public void observeHasIntermediateCuffPressureMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasIntermediateCuffPressureMeasurementStatus).observe(owner, observer);
    }

    public void observeIntermediateCuffPressureMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIntermediateCuffPressureMeasurementStatus).observe(owner, observer);
    }

    public void observeSupportIntermediateCuffPressureMeasurement(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(supportIntermediateCuffPressure).observe(owner, observer);
    }

    public void observeBloodPressureFeature(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBloodPressureFeature).observe(owner, observer);
    }

    @Nullable
    public String getBloodPressureMeasurementDataString() {
        return mBloodPressureMeasurementJson.getValue();
    }

    @MainThread
    public void setBloodPressureMeasurementDataString(@Nullable String bloodPressureMeasurementJson) {
        mBloodPressureMeasurementJson.setValue(bloodPressureMeasurementJson);
        if (bloodPressureMeasurementJson == null) {
            hasBloodPressureMeasurement.setValue(false);
            mBloodPressureMeasurementFlags.setValue(null);
            mBloodPressureMeasurementSystolic.setValue(null);
            mBloodPressureMeasurementDiastolic.setValue(null);
            mBloodPressureMeasurementMeanArterialPressure.setValue(null);
            hasBloodPressureMeasurementTimeStamp.setValue(false);
            mBloodPressureMeasurementTimeStamp.setValue(null);
            hasBloodPressureMeasurementPulseRate.setValue(false);
            mBloodPressureMeasurementPulseRate.setValue(null);
            hasBloodPressureMeasurementUserId.setValue(false);
            mBloodPressureMeasurementUserId.setValue(null);
            hasBloodPressureMeasurementMeasurementStatus.setValue(false);
            mBloodPressureMeasurementMeasurementStatus.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(bloodPressureMeasurementJson, CharacteristicData.class);
                hasBloodPressureMeasurement.setValue(true);
                BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
                mBloodPressureMeasurementFlags.setValue(Integer.toHexString(bloodPressureMeasurement.getFlags()));
                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                    mBloodPressureMeasurementSystolic.setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                } else {
                    mBloodPressureMeasurementSystolic.setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                }
                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                    mBloodPressureMeasurementDiastolic.setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat()));
                } else {
                    mBloodPressureMeasurementDiastolic.setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat()));
                }
                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                    mBloodPressureMeasurementMeanArterialPressure.setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat()));
                } else {
                    mBloodPressureMeasurementMeanArterialPressure.setValue(String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat()));
                }
                hasBloodPressureMeasurementTimeStamp.setValue(BloodPressureMeasurementUtils.isFlagsTimeStampPresent(bloodPressureMeasurement.getFlags()));
                mBloodPressureMeasurementTimeStamp.setValue(mResourceTextSource.getDateTimeString(bloodPressureMeasurement.getYear()
                        , bloodPressureMeasurement.getMonth()
                        , bloodPressureMeasurement.getDay()
                        , bloodPressureMeasurement.getHours()
                        , bloodPressureMeasurement.getMinutes()
                        , bloodPressureMeasurement.getSeconds()));
                hasBloodPressureMeasurementPulseRate.setValue(BloodPressureMeasurementUtils.isFlagsPulseRatePresent(bloodPressureMeasurement.getFlags()));
                mBloodPressureMeasurementPulseRate.setValue(String.valueOf(bloodPressureMeasurement.getPulseRate().getSfloat()));
                hasBloodPressureMeasurementUserId.setValue(BloodPressureMeasurementUtils.isFlagsUserIdPresent(bloodPressureMeasurement.getFlags()));
                mBloodPressureMeasurementUserId.setValue(String.valueOf(bloodPressureMeasurement.getUserId()));
                hasBloodPressureMeasurementMeasurementStatus.setValue(BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(bloodPressureMeasurement.getFlags()));
                mBloodPressureMeasurementMeasurementStatus.setValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0)));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    public String getIntermediateCuffPressureDataString() {
        return mIntermediateCuffPressureJson.getValue();
    }

    public void setIntermediateCuffPressureDataString(@Nullable String intermediateCuffPressureJson) {
        mIntermediateCuffPressureJson.setValue(intermediateCuffPressureJson);
        if (intermediateCuffPressureJson == null) {
            hasIntermediateCuffPressure.setValue(false);
            mIntermediateCuffPressureFlags.setValue(null);
            mIntermediateCuffPressureCurrentCuffPressure.setValue(null);
            hasIntermediateCuffPressureTimeStamp.setValue(false);
            mIntermediateCuffPressureTimeStamp.setValue(null);
            hasIntermediateCuffPressurePulseRate.setValue(false);
            mIntermediateCuffPressurePulseRate.setValue(null);
            hasIntermediateCuffPressureUserId.setValue(false);
            mIntermediateCuffPressureUserId.setValue(null);
            hasIntermediateCuffPressureMeasurementStatus.setValue(false);
            mIntermediateCuffPressureMeasurementStatus.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(intermediateCuffPressureJson, CharacteristicData.class);
                hasIntermediateCuffPressure.setValue(true);
                IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(characteristicData.data);
                mIntermediateCuffPressureFlags.setValue(Integer.toHexString(intermediateCuffPressure.getFlags()));
                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())) {
                    mIntermediateCuffPressureCurrentCuffPressure.setValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat()));
                } else {
                    mIntermediateCuffPressureCurrentCuffPressure.setValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat()));
                }
                hasIntermediateCuffPressureTimeStamp.setValue(BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags()));
                mIntermediateCuffPressureTimeStamp.setValue(mResourceTextSource.getDateTimeString(intermediateCuffPressure.getYear()
                        , intermediateCuffPressure.getMonth()
                        , intermediateCuffPressure.getDay()
                        , intermediateCuffPressure.getHours()
                        , intermediateCuffPressure.getMinutes()
                        , intermediateCuffPressure.getSeconds()));
                hasIntermediateCuffPressurePulseRate.setValue(BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags()));
                mIntermediateCuffPressurePulseRate.setValue(String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat()));
                hasIntermediateCuffPressureUserId.setValue(BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags()));
                mIntermediateCuffPressureUserId.setValue(String.valueOf(intermediateCuffPressure.getUserId()));
                hasIntermediateCuffPressureMeasurementStatus.setValue(BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags()));
                mIntermediateCuffPressureMeasurementStatus.setValue(Integer.toHexString(BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0)));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    public String getBloodPressureFeatureDataString() {
        return mBloodPressureFeatureJson.getValue();
    }

    public void setBloodPressureFeatureDataString(@Nullable String bloodPressureFeatureJson) {
        mBloodPressureFeatureJson.setValue(bloodPressureFeatureJson);
        if (bloodPressureFeatureJson == null) {
            hasBloodPressureFeature.setValue(false);
            mBloodPressureFeature.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(bloodPressureFeatureJson, CharacteristicData.class);
                hasBloodPressureFeature.setValue(true);
                BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(characteristicData.data);
                mBloodPressureFeature.setValue(Integer.toHexString(BLEUtils.createUInt16(bloodPressureFeature.getBloodPressureFeature(), 0)));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @NonNull
    @Override
    public Single<Optional<Intent>> save() {
        Intent intent;
        mServiceData.characteristicDataList.clear();
        if (Boolean.TRUE.equals(hasBloodPressureMeasurement.getValue())) {
            try {
                mServiceData.characteristicDataList.add(mGson.fromJson(mBloodPressureMeasurementJson.getValue(), CharacteristicData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        if (Boolean.TRUE.equals(hasIntermediateCuffPressure.getValue())) {
            try {
                mServiceData.characteristicDataList.add(mGson.fromJson(mIntermediateCuffPressureJson.getValue(), CharacteristicData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
        if (Boolean.TRUE.equals(hasBloodPressureFeature.getValue())) {
            try {
                mServiceData.characteristicDataList.add(mGson.fromJson(mBloodPressureFeatureJson.getValue(), CharacteristicData.class));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }

        if (mServiceData.characteristicDataList
                .stream()
                .filter(characteristicData -> !characteristicData.uuid.equals(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC)).count() == 2) {
            intent = new Intent();
            intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(mServiceData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }

}