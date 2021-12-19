package org.im97mori.ble.android.peripheral.ui.device.setting.u2a36;

import static org.im97mori.ble.constants.CharacteristicUUID.INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;

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
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.im97mori.ble.descriptor.u2902.ClientCharacteristicConfiguration;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

public class IntermediateCuffPressureSettingViewModel extends BaseCharacteristicViewModel {

    private final MutableLiveData<Boolean> isMmhg;
    private final MutableLiveData<String> unit;

    private final MutableLiveData<String> currentCuffPressure;
    private final MutableLiveData<String> currentCuffPressureError;
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

    private final MutableLiveData<String> notificationCount;
    private final MutableLiveData<String> notificationCountError;

    protected final MutableLiveData<String> mClientCharacteristicConfigurationJson;

    public IntermediateCuffPressureSettingViewModel(@NonNull SavedStateHandle savedStateHandle) {
        isMmhg = savedStateHandle.getLiveData("isMmhg");
        unit = savedStateHandle.getLiveData("unit");

        currentCuffPressure = savedStateHandle.getLiveData("currentCuffPressure");
        currentCuffPressureError = savedStateHandle.getLiveData("currentCuffPressureError");
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

        notificationCount = savedStateHandle.getLiveData("notificationCount");
        notificationCountError = savedStateHandle.getLiveData("notificationCountError");

        mClientCharacteristicConfigurationJson = savedStateHandle.getLiveData("mClientCharacteristicConfiguration");
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        Completable completable;
        if (mCharacteristicData == null) {
            completable = Single.just(Optional.ofNullable(intent.getStringExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString())))
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
                            mCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
                            mCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
                        }

                        IntermediateCuffPressure intermediateCuffPressure;
                        if (mCharacteristicData.data == null) {
                            intermediateCuffPressure = null;
                        } else {
                            intermediateCuffPressure = new IntermediateCuffPressure(mCharacteristicData.data);
                        }

                        if (intermediateCuffPressure == null) {
                            unit.postValue(mResourceTextSource.getUnitString(true));
                            isMmhg.postValue(true);
                            hasTimeStamp.postValue(false);
                            hasPulseRate.postValue(false);
                            hasUserId.postValue(false);
                            hasMeasurementStatus.postValue(false);
                        } else {
                            unit.postValue(mResourceTextSource.getUnitString(BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())));
                            isMmhg.postValue(BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags()));
                            hasTimeStamp.postValue(BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags()));
                            hasPulseRate.postValue(BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags()));
                            hasUserId.postValue(BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags()));
                            hasMeasurementStatus.postValue(BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags()));
                        }

                        if (this.currentCuffPressure.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                currentCuffPressureError.postValue(mResourceTextSource.getSystolicErrorString(null));
                            } else {
                                String text;
                                if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags())) {
                                    text = String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat());
                                } else {
                                    text = String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat());
                                }
                                this.currentCuffPressure.postValue(text);
                                currentCuffPressureError.postValue(mResourceTextSource.getSystolicErrorString(text));
                            }
                        } else {
                            currentCuffPressureError.postValue(mResourceTextSource.getSystolicErrorString(currentCuffPressure.getValue()));
                        }

                        if (this.timeStampYear.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                timeStampYearError.postValue(mResourceTextSource.getSystolicErrorString(null));
                            } else {
                                String text = String.valueOf(intermediateCuffPressure.getYear());
                                this.timeStampYear.postValue(text);
                                timeStampYearError.postValue(mResourceTextSource.getMeanArterialPressureErrorString(text));
                            }
                        } else {
                            timeStampYearError.postValue(mResourceTextSource.getDateTimeYearErrorString(timeStampYear.getValue()));
                        }

                        if (this.timeStampMonth.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                timeStampMonth.postValue(0);
                            } else {
                                List<Pair<Integer, String>> list = provideDateTimeMonthList();
                                Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == intermediateCuffPressure.getMonth());
                                if (optional.isPresent()) {
                                    timeStampMonth.postValue(list.indexOf(optional.get()));
                                } else {
                                    timeStampMonth.postValue(0);
                                }
                            }
                        }

                        if (this.timeStampDay.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                timeStampDay.postValue(0);
                            } else {
                                List<Pair<Integer, String>> list = provideDateTimeDayList();
                                Optional<Pair<Integer, String>> optional = list.stream().findFirst().filter(integerStringPair -> integerStringPair.first == intermediateCuffPressure.getDay());
                                if (optional.isPresent()) {
                                    timeStampDay.postValue(list.indexOf(optional.get()));
                                } else {
                                    timeStampDay.postValue(0);
                                }
                            }
                        }

                        if (this.timeStampHours.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                timeStampHours.postValue(0);
                            } else {
                                List<String> list = provideDateTimeHoursList();
                                timeStampHours.postValue(list.indexOf(String.valueOf(intermediateCuffPressure.getHours())));
                            }
                        }

                        if (this.timeStampMinutes.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                timeStampMinutes.postValue(0);
                            } else {
                                List<String> list = provideDateTimeMinutesList();
                                timeStampMinutes.postValue(list.indexOf(String.valueOf(intermediateCuffPressure.getMinutes())));
                            }
                        }

                        if (this.timeStampSeconds.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                timeStampSeconds.postValue(0);
                            } else {
                                List<String> list = provideDateTimeSecondsList();
                                timeStampSeconds.postValue(list.indexOf(String.valueOf(intermediateCuffPressure.getSeconds())));
                            }
                        }

                        if (this.pulseRate.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                pulseRateError.postValue(mResourceTextSource.getPulseRateErrorString(null));
                            } else {
                                String text = String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat());
                                this.pulseRate.postValue(text);
                                pulseRateError.postValue(mResourceTextSource.getPulseRateErrorString(text));
                            }
                        } else {
                            pulseRateError.postValue(mResourceTextSource.getDateTimeYearErrorString(pulseRate.getValue()));
                        }

                        if (this.userId.getValue() == null) {
                            if (intermediateCuffPressure == null) {
                                userIdError.postValue(mResourceTextSource.getUserIdErrorString(null));
                            } else {
                                String text = String.valueOf(intermediateCuffPressure.getUserId());
                                this.userId.postValue(text);
                                userIdError.postValue(mResourceTextSource.getUserIdErrorString(text));
                            }
                        } else {
                            userIdError.postValue(mResourceTextSource.getUserIdErrorString(userId.getValue()));
                        }

                        int measurementStatus;
                        if (intermediateCuffPressure == null) {
                            measurementStatus = 0;
                        } else {
                            measurementStatus = BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0);
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
                                    mClientCharacteristicConfiguration.postValue(mResourceTextSource.getNotificationsString(new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesNotificationsEnabled()));
                                } catch (JsonSyntaxException e) {
                                    e.printStackTrace();
                                }
                            }
                        }

                        if (notificationCount.getValue() == null) {
                            notificationCount.postValue(String.valueOf(mCharacteristicData.notificationCount));
                            notificationCountError.postValue(mResourceTextSource.getNotificationCountErrorString(String.valueOf(mCharacteristicData.notificationCount)));
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

    public void observeCurrentCuffPressure(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(currentCuffPressure).observe(owner, observer);
    }

    public void observeCurrentCuffPressureError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(currentCuffPressureError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateCurrentCuffPressure(@NonNull CharSequence text) {
        currentCuffPressure.setValue(text.toString());
        currentCuffPressureError.setValue(mResourceTextSource.getCurrentCuffPressureErrorString(text));
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
                mClientCharacteristicConfiguration.postValue(mResourceTextSource.getNotificationsString(clientCharacteristicConfiguration.isPropertiesIndicationsEnabled()));
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    public String getClientCharacteristicConfigurationDescriptorDataString() {
        return mClientCharacteristicConfigurationJson.getValue();
    }

    public void observeNotificationCount(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(notificationCount).observe(owner, observer);
    }

    public void observeNotificationCountError(@NonNull LifecycleOwner owner, @NonNull Observer<CharSequence> observer) {
        Transformations.distinctUntilChanged(notificationCountError).observe(owner, observer);
    }

    @MainThread
    public synchronized void updateNotificationCount(@NonNull CharSequence text) {
        notificationCount.setValue(text.toString());
        notificationCountError.setValue(mResourceTextSource.getNotificationCountErrorString(text));
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
        if (TextUtils.isEmpty(currentCuffPressureError.getValue())
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
            IEEE_11073_20601_SFLOAT systolicKpa;
            if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(flags)) {
                systolicMmhg = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(currentCuffPressure.getValue())));

                systolicKpa = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
            } else {
                systolicMmhg = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);

                systolicKpa = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(currentCuffPressure.getValue())));
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

            mCharacteristicData.data = new IntermediateCuffPressure(flags
                    , systolicMmhg
                    , systolicKpa
                    , new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN)
                    , new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN)
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
            mCharacteristicData.notificationCount = Integer.parseInt(Objects.requireNonNull(notificationCount.getValue()));

            intent = new Intent();
            intent.putExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), mGson.toJson(mCharacteristicData));
        } else {
            intent = null;
        }
        return Single.just(Optional.ofNullable(intent));
    }

}