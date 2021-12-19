package org.im97mori.ble.android.peripheral.ui.device.setting.u2a35;

import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;

import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
import org.im97mori.ble.descriptor.u2902.ClientCharacteristicConfiguration;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class BloodPressureMeasurementSettingViewModel extends BaseCharacteristicViewModel {

    private final MutableLiveData<Boolean> isMmhg;
    private final MutableLiveData<String> unit;

    private final MutableLiveData<String> systolic;
    private final MutableLiveData<String> systolicError;
    private final MutableLiveData<String> diastolic;
    private final MutableLiveData<String> diastolicError;
    private final MutableLiveData<String> meanArterialPressure;
    private final MutableLiveData<String> meanArterialPressureError;
    private final MutableLiveData<Boolean> hasTimeStamp;
    private final MutableLiveData<String> timeStampYear;
    private final MutableLiveData<String> timeStampYearError;
    private final MutableLiveData<Integer> timeStampMonth;
    private final MutableLiveData<Integer> timeStampDay;
    private final MutableLiveData<Integer> timeStampHours;
    private final MutableLiveData<Integer> timeStampMinutes;
    private final MutableLiveData<Integer> timeStampSeconds;
    private final MutableLiveData<Boolean> hasPulseRate;
    private final MutableLiveData<String> pulseRate;
    private final MutableLiveData<String> pulseRateError;
    private final MutableLiveData<Boolean> hasUserId;
    private final MutableLiveData<String> userId;
    private final MutableLiveData<String> userIdError;
    private final MutableLiveData<Boolean> hasMeasurementStatus;
    private final MutableLiveData<Integer> bodyMovementDetection;
    private final MutableLiveData<Integer> cuffFitDetection;
    private final MutableLiveData<Integer> irregularPulseDetection;
    private final MutableLiveData<Integer> pulseRateRangeDetection;
    private final MutableLiveData<Integer> measurementPositionDetection;

    private final MutableLiveData<Boolean> hasClientCharacteristicConfiguration;
    protected final MutableLiveData<String> mClientCharacteristicConfiguration;

    private final MutableLiveData<String> indicationCount;
    private final MutableLiveData<String> indicationCountError;

    protected final MutableLiveData<String> mClientCharacteristicConfigurationJson;

    public BloodPressureMeasurementSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        isMmhg = savedStateHandle.getLiveData("isMmhg");
        unit = savedStateHandle.getLiveData("unit");

        systolic = savedStateHandle.getLiveData("systolic");
        systolicError = savedStateHandle.getLiveData("systolicError");
        diastolic = savedStateHandle.getLiveData("diastolic");
        diastolicError = savedStateHandle.getLiveData("diastolicError");
        meanArterialPressure = savedStateHandle.getLiveData("meanArterialPressure");
        meanArterialPressureError = savedStateHandle.getLiveData("meanArterialPressureError");
        hasTimeStamp = savedStateHandle.getLiveData("hasTimeStamp");
        timeStampYear = savedStateHandle.getLiveData("timeStampYear");
        timeStampYearError = savedStateHandle.getLiveData("timeStampYearError");
        timeStampMonth = savedStateHandle.getLiveData("timeStampMonth");
        timeStampDay = savedStateHandle.getLiveData("timeStampDay");
        timeStampHours = savedStateHandle.getLiveData("timeStampHours");
        timeStampMinutes = savedStateHandle.getLiveData("timeStampMinutes");
        timeStampSeconds = savedStateHandle.getLiveData("timeStampSeconds");
        hasPulseRate = savedStateHandle.getLiveData("hasPulseRate");
        pulseRate = savedStateHandle.getLiveData("pulseRate");
        pulseRateError = savedStateHandle.getLiveData("pulseRateError");
        hasUserId = savedStateHandle.getLiveData("hasUserId");
        userId = savedStateHandle.getLiveData("userId");
        userIdError = savedStateHandle.getLiveData("userIdError");
        hasMeasurementStatus = savedStateHandle.getLiveData("hasMeasurementStatus");
        bodyMovementDetection = savedStateHandle.getLiveData("bodyMovementDetection");
        cuffFitDetection = savedStateHandle.getLiveData("cuffFitDetection");
        irregularPulseDetection = savedStateHandle.getLiveData("irregularPulseDetection");
        pulseRateRangeDetection = savedStateHandle.getLiveData("pulseRateRangeDetection");
        measurementPositionDetection = savedStateHandle.getLiveData("measurementPositionDetection");

        hasClientCharacteristicConfiguration = savedStateHandle.getLiveData("hasClientCharacteristicConfiguration");
        mClientCharacteristicConfiguration = savedStateHandle.getLiveData("mClientCharacteristicConfiguration");

        indicationCount = savedStateHandle.getLiveData("indicationCount");
        indicationCountError = savedStateHandle.getLiveData("indicationCountError");

        mClientCharacteristicConfigurationJson = savedStateHandle.getLiveData("mClientCharacteristicConfigurationJson");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mCharacteristicData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString())))
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
                            mCharacteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
                            mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
                        }

                        BloodPressureMeasurement bloodPressureMeasurement;
                        if (mCharacteristicData.data == null) {
                            bloodPressureMeasurement = null;
                        } else {
                            bloodPressureMeasurement = new BloodPressureMeasurement(mCharacteristicData.data);
                        }

                        if (bloodPressureMeasurement == null) {
                            unit.postValue(mResourceTextSource.getUnitString(true));
                            isMmhg.postValue(true);
                            hasTimeStamp.postValue(false);
                            hasPulseRate.postValue(false);
                            hasUserId.postValue(false);
                            hasMeasurementStatus.postValue(false);
                        } else {
                            unit.postValue(mResourceTextSource.getUnitString(BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())));
                            isMmhg.postValue(BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags()));
                            hasTimeStamp.postValue(BloodPressureMeasurementUtils.isFlagsTimeStampPresent(bloodPressureMeasurement.getFlags()));
                            hasPulseRate.postValue(BloodPressureMeasurementUtils.isFlagsPulseRatePresent(bloodPressureMeasurement.getFlags()));
                            hasUserId.postValue(BloodPressureMeasurementUtils.isFlagsUserIdPresent(bloodPressureMeasurement.getFlags()));
                            hasMeasurementStatus.postValue(BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(bloodPressureMeasurement.getFlags()));
                        }

                        if (this.systolic.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                systolicError.postValue(mResourceTextSource.getSystolicErrorString(null));
                            } else {
                                String text;
                                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                                    text = String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat());
                                } else {
                                    text = String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicKpa().getSfloat());
                                }
                                this.systolic.postValue(text);
                                systolicError.postValue(mResourceTextSource.getSystolicErrorString(text));
                            }
                        } else {
                            systolicError.postValue(mResourceTextSource.getSystolicErrorString(systolic.getValue()));
                        }

                        if (this.diastolic.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                diastolicError.postValue(mResourceTextSource.getSystolicErrorString(null));
                            } else {
                                String text;
                                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                                    text = String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat());
                                } else {
                                    text = String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat());
                                }
                                this.diastolic.postValue(text);
                                diastolicError.postValue(mResourceTextSource.getDiastolicErrorString(text));
                            }
                        } else {
                            diastolicError.postValue(mResourceTextSource.getDiastolicErrorString(diastolic.getValue()));
                        }

                        if (this.meanArterialPressure.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                meanArterialPressureError.postValue(mResourceTextSource.getSystolicErrorString(null));
                            } else {
                                String text;
                                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(bloodPressureMeasurement.getFlags())) {
                                    text = String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat());
                                } else {
                                    text = String.valueOf(bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat());
                                }
                                this.meanArterialPressure.postValue(text);
                                meanArterialPressureError.postValue(mResourceTextSource.getMeanArterialPressureErrorString(text));
                            }
                        } else {
                            meanArterialPressureError.postValue(mResourceTextSource.getMeanArterialPressureErrorString(meanArterialPressure.getValue()));
                        }

                        if (this.timeStampYear.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                timeStampYearError.postValue(mResourceTextSource.getSystolicErrorString(null));
                            } else {
                                String text = String.valueOf(bloodPressureMeasurement.getYear());
                                this.timeStampYear.postValue(text);
                                timeStampYearError.postValue(mResourceTextSource.getMeanArterialPressureErrorString(text));
                            }
                        } else {
                            timeStampYearError.postValue(mResourceTextSource.getDateTimeYearErrorString(timeStampYear.getValue()));
                        }

                        if (this.timeStampMonth.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                timeStampMonth.postValue(0);
                            } else {
                                List<Pair<Integer, String>> list = provideDateTimeMonthList();
                                Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == bloodPressureMeasurement.getMonth());
                                if (optional.isPresent()) {
                                    timeStampMonth.postValue(list.indexOf(optional.get()));
                                } else {
                                    timeStampMonth.postValue(0);
                                }
                            }
                        }

                        if (this.timeStampDay.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                timeStampDay.postValue(0);
                            } else {
                                List<Pair<Integer, String>> list = provideDateTimeDayList();
                                Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == bloodPressureMeasurement.getDay());
                                if (optional.isPresent()) {
                                    timeStampDay.postValue(list.indexOf(optional.get()));
                                } else {
                                    timeStampDay.postValue(0);
                                }
                            }
                        }

                        if (this.timeStampHours.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                timeStampHours.postValue(0);
                            } else {
                                List<String> list = provideDateTimeHoursList();
                                timeStampHours.postValue(list.indexOf(String.valueOf(bloodPressureMeasurement.getHours())));
                            }
                        }

                        if (this.timeStampMinutes.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                timeStampMinutes.postValue(0);
                            } else {
                                List<String> list = provideDateTimeMinutesList();
                                timeStampMinutes.postValue(list.indexOf(String.valueOf(bloodPressureMeasurement.getMinutes())));
                            }
                        }

                        if (this.timeStampSeconds.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                timeStampSeconds.postValue(0);
                            } else {
                                List<String> list = provideDateTimeSecondsList();
                                timeStampSeconds.postValue(list.indexOf(String.valueOf(bloodPressureMeasurement.getSeconds())));
                            }
                        }

                        if (this.pulseRate.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                pulseRateError.postValue(mResourceTextSource.getPulseRateErrorString(null));
                            } else {
                                String text = String.valueOf(bloodPressureMeasurement.getPulseRate().getSfloat());
                                this.pulseRate.postValue(text);
                                pulseRateError.postValue(mResourceTextSource.getPulseRateErrorString(text));
                            }
                        } else {
                            pulseRateError.postValue(mResourceTextSource.getPulseRateErrorString(pulseRate.getValue()));
                        }

                        if (this.userId.getValue() == null) {
                            if (bloodPressureMeasurement == null) {
                                userIdError.postValue(mResourceTextSource.getUserIdErrorString(null));
                            } else {
                                String text = String.valueOf(bloodPressureMeasurement.getUserId());
                                this.userId.postValue(text);
                                userIdError.postValue(mResourceTextSource.getUserIdErrorString(text));
                            }
                        } else {
                            userIdError.postValue(mResourceTextSource.getUserIdErrorString(userId.getValue()));
                        }

                        int measurementStatus;
                        if (bloodPressureMeasurement == null) {
                            measurementStatus = 0;
                        } else {
                            measurementStatus = BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0);
                        }

                        if (this.bodyMovementDetection.getValue() == null) {
                            List<Pair<Integer, String>> list = provideBodyMovementDetectionList();
                            int maskedBodyMovementDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == maskedBodyMovementDetection);
                            if (optional.isPresent()) {
                                bodyMovementDetection.postValue(list.indexOf(optional.get()));
                            } else {
                                bodyMovementDetection.postValue(0);
                            }
                        }

                        if (this.cuffFitDetection.getValue() == null) {
                            List<Pair<Integer, String>> list = provideCuffFitDetectionList();
                            int maskedCuffFitDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == maskedCuffFitDetection);
                            if (optional.isPresent()) {
                                cuffFitDetection.postValue(list.indexOf(optional.get()));
                            } else {
                                cuffFitDetection.postValue(0);
                            }
                        }

                        if (this.irregularPulseDetection.getValue() == null) {
                            List<Pair<Integer, String>> list = provideIrregularPulseDetectionList();
                            int maskedIrregularPulseDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == maskedIrregularPulseDetection);
                            if (optional.isPresent()) {
                                irregularPulseDetection.postValue(list.indexOf(optional.get()));
                            } else {
                                irregularPulseDetection.postValue(0);
                            }
                        }

                        if (this.pulseRateRangeDetection.getValue() == null) {
                            List<Pair<Integer, String>> list = providePulseRateRangeDetectionList();
                            int maskedPulseRateRangeDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == maskedPulseRateRangeDetection);
                            if (optional.isPresent()) {
                                pulseRateRangeDetection.postValue(list.indexOf(optional.get()));
                            } else {
                                pulseRateRangeDetection.postValue(0);
                            }
                        }

                        if (this.measurementPositionDetection.getValue() == null) {
                            List<Pair<Integer, String>> list = provideMeasurementPositionDetectionList();
                            int maskedMeasurementPositionDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == maskedMeasurementPositionDetection);
                            if (optional.isPresent()) {
                                measurementPositionDetection.postValue(list.indexOf(optional.get()));
                            } else {
                                measurementPositionDetection.postValue(0);
                            }
                        }

                        if (hasClientCharacteristicConfiguration.getValue() == null) {
                            hasClientCharacteristicConfiguration.postValue(!mCharacteristicData.descriptorDataList.isEmpty());
                        }

                        if (mClientCharacteristicConfiguration.getValue() == null) {
                            if (mClientCharacteristicConfigurationJson.getValue() != null) {
                                try {
                                    DescriptorData descriptorData = mGson.fromJson(mClientCharacteristicConfigurationJson.getValue(), DescriptorData.class);
                                    mClientCharacteristicConfiguration.postValue(mResourceTextSource.getIndicationsString(new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesIndicationsDisabled()));
                                } catch (JsonSyntaxException e) {
                                    e.printStackTrace();
                                }
                            }
                        }

                        if (indicationCount.getValue() == null) {
                            indicationCount.postValue(String.valueOf(mCharacteristicData.notificationCount));
                            indicationCountError.postValue(mResourceTextSource.getNotificationCountErrorString(String.valueOf(mCharacteristicData.notificationCount)));
                        }

                        return Completable.complete();
                    });
        } else {
            completable = Completable.complete();
        }
        return completable;
    }

    public void observeIsMmhg(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(isMmhg).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIsMmhg(@NonNull Boolean text) {
        isMmhg.setValue(text);
        unit.postValue(mResourceTextSource.getUnitString(text));
    }


    public void observeUnit(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(unit).observe(owner, observer);
    }

    public void observeSystolic(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(systolic).observe(owner, observer);
    }

    public void observeSystolicError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(systolicError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateSystolic(@NonNull CharSequence text) {
        systolic.setValue(text.toString());
        systolicError.setValue(mResourceTextSource.getSystolicErrorString(text));
    }

    public void observeDiastolic(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(diastolic).observe(owner, observer);
    }

    public void observeDiastolicError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(diastolicError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateDiastolic(@NonNull CharSequence text) {
        diastolic.setValue(text.toString());
        diastolicError.setValue(mResourceTextSource.getDiastolicErrorString(text));
    }

    public void observeMeanArterialPressure(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(meanArterialPressure).observe(owner, observer);
    }

    public void observeMeanArterialPressureError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(meanArterialPressureError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateMeanArterialPressure(@NonNull CharSequence text) {
        meanArterialPressure.setValue(text.toString());
        meanArterialPressureError.setValue(mResourceTextSource.getMeanArterialPressureErrorString(text));
    }

    public void observeHasTimeStamp(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasTimeStamp).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateHasTimeStamp(Boolean checked) {
        hasTimeStamp.setValue(checked);
    }

    public void observeTimeStampYear(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(timeStampYear).observe(owner, observer);
    }

    public void observeTimeStampYearError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(timeStampYearError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateTimeStampYear(@NonNull CharSequence text) {
        timeStampYear.setValue(text.toString());
        timeStampYearError.setValue(mResourceTextSource.getDateTimeYearErrorString(text));
    }

    public void observeTimeStampMonth(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(timeStampMonth).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateTimeStampMonth(@NonNull Integer text) {
        timeStampMonth.setValue(text);
    }

    public void observeTimeStampDay(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(timeStampDay).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateTimeStampDay(@NonNull Integer text) {
        timeStampDay.setValue(text);
    }

    public void observeTimeStampHours(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(timeStampHours).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateTimeStampHours(@NonNull Integer text) {
        timeStampHours.setValue(text);
    }

    public void observeTimeStampMinutes(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(timeStampMinutes).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateTimeStampMinutes(@NonNull Integer text) {
        timeStampMinutes.setValue(text);
    }

    public void observeTimeStampSeconds(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(timeStampSeconds).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateTimeStampSeconds(@NonNull Integer text) {
        timeStampSeconds.setValue(text);
    }

    public void observeHasPulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasPulseRate).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateHasPulseRate(Boolean checked) {
        hasPulseRate.setValue(checked);
    }

    public void observePulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(pulseRate).observe(owner, observer);
    }

    public void observePulseRateError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(pulseRateError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updatePulseRate(@NonNull CharSequence text) {
        pulseRate.setValue(text.toString());
        pulseRateError.setValue(mResourceTextSource.getPulseRateErrorString(text));
    }

    public void observeHasUserId(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasUserId).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateHasUserId(Boolean checked) {
        hasUserId.setValue(checked);
    }

    public void observeUserId(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(userId).observe(owner, observer);
    }

    public void observeUserIdError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(userIdError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateUserId(@NonNull CharSequence text) {
        userId.setValue(text.toString());
        userIdError.setValue(mResourceTextSource.getUserIdErrorString(text));
    }

    public void observeHasMeasurementStatus(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasMeasurementStatus).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateHasMeasurementStatus(Boolean checked) {
        hasMeasurementStatus.setValue(checked);
    }

    public void observeBodyMovementDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(bodyMovementDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateBodyMovementDetection(@NonNull Integer text) {
        bodyMovementDetection.setValue(text);
    }

    public void observeCuffFitDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(cuffFitDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateCuffFitDetection(@NonNull Integer text) {
        cuffFitDetection.setValue(text);
    }

    public void observeIrregularPulseDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(irregularPulseDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIrregularPulseDetection(@NonNull Integer text) {
        irregularPulseDetection.setValue(text);
    }

    public void observePulseRateRangeDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(pulseRateRangeDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updatePulseRateRangeDetection(@NonNull Integer text) {
        pulseRateRangeDetection.setValue(text);
    }

    public void observeMeasurementPositionDetection(@NonNull LifecycleOwner owner, @NonNull Observer<Integer> observer) {
        Transformations.distinctUntilChanged(measurementPositionDetection).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateMeasurementPositionDetection(@NonNull Integer text) {
        measurementPositionDetection.setValue(text);
    }

    public void observeHasClientCharacteristicConfiguration(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(hasClientCharacteristicConfiguration).observe(owner, observer);
    }

    public void observeClientCharacteristicConfiguration(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mClientCharacteristicConfiguration).observe(owner, observer);
    }

    @MainThread
    public void setClientCharacteristicConfigurationDescriptorDataString(@Nullable String clientCharacteristicConfigurationJson) {
        mClientCharacteristicConfigurationJson.setValue(clientCharacteristicConfigurationJson);
        if (clientCharacteristicConfigurationJson == null) {
            hasClientCharacteristicConfiguration.setValue(false);
            mClientCharacteristicConfiguration.setValue(null);
        } else {
            try {
                DescriptorData descriptorData = mGson.fromJson(clientCharacteristicConfigurationJson, DescriptorData.class);
                hasClientCharacteristicConfiguration.setValue(true);
                ClientCharacteristicConfiguration clientCharacteristicConfiguration = new ClientCharacteristicConfiguration(descriptorData.data);
                mClientCharacteristicConfiguration.postValue(mResourceTextSource.getIndicationsString(clientCharacteristicConfiguration.isPropertiesIndicationsEnabled()));
                hasClientCharacteristicConfiguration.postValue(true);
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    public String getClientCharacteristicConfigurationDescriptorDataString() {
        return mClientCharacteristicConfigurationJson.getValue();
    }

    public void observeIndicationCount(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(indicationCount).observe(owner, observer);
    }

    public void observeIndicationCountError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(indicationCountError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateIndicationCount(@NonNull CharSequence text) {
        indicationCount.setValue(text.toString());
        indicationCountError.setValue(mResourceTextSource.getIndicationCountErrorString(text));
    }

    @NonNull
    public List<Pair<Integer, String>> provideDateTimeMonthList() {
        return mResourceTextSource.provideDateTimeMonthList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideDateTimeDayList() {
        return mResourceTextSource.provideDateTimeDayList();
    }

    @NonNull
    public List<String> provideDateTimeHoursList() {
        return mResourceTextSource.provideDateTimeHoursList();
    }

    @NonNull
    public List<String> provideDateTimeMinutesList() {
        return mResourceTextSource.provideDateTimeMinutesList();
    }

    @NonNull
    public List<String> provideDateTimeSecondsList() {
        return mResourceTextSource.provideDateTimeSecondsList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideBodyMovementDetectionList() {
        return mResourceTextSource.provideBodyMovementDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideCuffFitDetectionList() {
        return mResourceTextSource.provideCuffFitDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideIrregularPulseDetectionList() {
        return mResourceTextSource.provideIrregularPulseDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> providePulseRateRangeDetectionList() {
        return mResourceTextSource.providePulseRateRangeDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideMeasurementPositionDetectionList() {
        return mResourceTextSource.provideMeasurementPositionDetectionList();
    }

    @NonNull
    @Override
    public Single<Optional<Intent>> save() {
        Intent intent;
        if (TextUtils.isEmpty(systolicError.getValue())
                && TextUtils.isEmpty(diastolicError.getValue())
                && TextUtils.isEmpty(meanArterialPressureError.getValue())
                && TextUtils.isEmpty(timeStampYearError.getValue())
                && TextUtils.isEmpty(pulseRateError.getValue())
                && TextUtils.isEmpty(userIdError.getValue())
                && Boolean.TRUE.equals(hasClientCharacteristicConfiguration.getValue())) {
            int flags = 0;
            if (Boolean.FALSE.equals(isMmhg.getValue())) {
                flags |= BloodPressureMeasurementUtils.FLAG_BLOOD_PRESSURE_UNITS_KPA;
            }
            if (Boolean.TRUE.equals(hasTimeStamp.getValue())) {
                flags |= BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT;
            }
            if (Boolean.TRUE.equals(hasUserId.getValue())) {
                flags |= BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT;
            }
            if (Boolean.TRUE.equals(hasMeasurementStatus.getValue())) {
                flags |= BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
            }

            IEEE_11073_20601_SFLOAT systolicMmhg;
            IEEE_11073_20601_SFLOAT diastolicMmhg;
            IEEE_11073_20601_SFLOAT meanArterialPressureMmhg;
            IEEE_11073_20601_SFLOAT systolicKpa;
            IEEE_11073_20601_SFLOAT diastolicKpa;
            IEEE_11073_20601_SFLOAT meanArterialPressureKpa;
            if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(flags)) {
                systolicMmhg = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(systolic.getValue())));
                diastolicMmhg = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(diastolic.getValue())));
                meanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(meanArterialPressure.getValue())));

                systolicKpa = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
                diastolicKpa = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
                meanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
            } else {
                systolicMmhg = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
                diastolicMmhg = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
                meanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);

                systolicKpa = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(systolic.getValue())));
                diastolicKpa = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(diastolic.getValue())));
                meanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(meanArterialPressure.getValue())));
            }

            int year;
            int month;
            int day;
            int hours;
            int minutes;
            int seconds;
            if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(flags)) {
                year = Integer.parseInt(Objects.requireNonNull(timeStampYear.getValue()));
                month = provideDateTimeMonthList().get(Objects.requireNonNull(timeStampMonth.getValue())).first;
                day = provideDateTimeDayList().get(Objects.requireNonNull(timeStampDay.getValue())).first;
                hours = Integer.parseInt(provideDateTimeHoursList().get(Objects.requireNonNull(timeStampHours.getValue())));
                minutes = Integer.parseInt(provideDateTimeMinutesList().get(Objects.requireNonNull(timeStampMinutes.getValue())));
                seconds = Integer.parseInt(provideDateTimeSecondsList().get(Objects.requireNonNull(timeStampSeconds.getValue())));
            } else {
                year = 0;
                month = 0;
                day = 0;
                hours = 0;
                minutes = 0;
                seconds = 0;
            }

            IEEE_11073_20601_SFLOAT pulseRate;
            if (Boolean.TRUE.equals(hasPulseRate.getValue())) {
                pulseRate = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(this.pulseRate.getValue())));
            } else {
                pulseRate = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
            }

            int userId;
            if (Boolean.TRUE.equals(hasUserId.getValue())) {
                userId = Integer.parseInt(Objects.requireNonNull(this.userId.getValue()));
            } else {
                userId = 0;
            }

            byte[] measurementStatus;
            if (Boolean.TRUE.equals(hasMeasurementStatus.getValue())) {
                int measurementStatusFlags = provideBodyMovementDetectionList().get(Objects.requireNonNull(bodyMovementDetection.getValue())).first;
                measurementStatusFlags |= provideCuffFitDetectionList().get(Objects.requireNonNull(cuffFitDetection.getValue())).first;
                measurementStatusFlags |= provideIrregularPulseDetectionList().get(Objects.requireNonNull(irregularPulseDetection.getValue())).first;
                measurementStatusFlags |= providePulseRateRangeDetectionList().get(Objects.requireNonNull(pulseRateRangeDetection.getValue())).first;
                measurementStatusFlags |= provideMeasurementPositionDetectionList().get(Objects.requireNonNull(measurementPositionDetection.getValue())).first;

                measurementStatus = new byte[2];
                measurementStatus[0] = (byte) measurementStatusFlags;
                measurementStatus[1] = (byte) (measurementStatusFlags >> 8);
            } else {
                measurementStatus = new byte[0];
            }

            mCharacteristicData.data = new BloodPressureMeasurement(flags
                    , systolicMmhg
                    , diastolicMmhg
                    , meanArterialPressureMmhg
                    , systolicKpa
                    , diastolicKpa
                    , meanArterialPressureKpa
                    , year
                    , month
                    , day
                    , hours
                    , minutes
                    , seconds
                    , pulseRate
                    , userId
                    , measurementStatus).getBytes();
            mCharacteristicData.descriptorDataList.clear();
            mCharacteristicData.descriptorDataList.add(mGson.fromJson(mClientCharacteristicConfigurationJson.getValue(), DescriptorData.class));
            mCharacteristicData.notificationCount = Integer.parseInt(Objects.requireNonNull(indicationCount.getValue()));

            intent = new Intent();
            intent.putExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }

}