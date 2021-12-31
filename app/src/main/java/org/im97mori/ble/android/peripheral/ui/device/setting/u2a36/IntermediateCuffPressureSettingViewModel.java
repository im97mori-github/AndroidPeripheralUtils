package org.im97mori.ble.android.peripheral.ui.device.setting.u2a36;

import static org.im97mori.ble.constants.CharacteristicUUID.INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;

import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseCharacteristicViewModel;
import org.im97mori.ble.android.peripheral.utils.ExistObserver;
import org.im97mori.ble.android.peripheral.utils.MapObserver;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.core.DateTimeUtils;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.characteristic.core.UserIndexUtils;
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.im97mori.ble.descriptor.u2902.ClientCharacteristicConfiguration;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class IntermediateCuffPressureSettingViewModel extends BaseCharacteristicViewModel {

    private static final String KEY_IS_MMHG = "KEY_IS_MMHG";
    private static final String KEY_IS_TIME_STAMP_SUPPORTED = "KEY_IS_TIME_STAMP_SUPPORTED";
    private static final String KEY_IS_PULSE_RATE_SUPPORTED = "KEY_IS_PULSE_RATE_SUPPORTED";
    private static final String KEY_IS_USER_ID_SUPPORTED = "KEY_IS_USER_ID_SUPPORTED";
    private static final String KEY_IS_MEASUREMENT_STATUS_SUPPORTED = "KEY_IS_MEASUREMENT_STATUS_SUPPORTED";

    private static final String KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON = "KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON";

    private static final String KEY_CURRENT_CUFF_PRESSURE = "KEY_CURRENT_CUFF_PRESSURE";
    private static final String KEY_TIME_STAMP_YEAR = "KEY_TIME_STAMP_YEAR";
    private static final String KEY_TIME_STAMP_MONTH = "KEY_TIME_STAMP_MONTH";
    private static final String KEY_TIME_STAMP_DAY = "KEY_TIME_STAMP_DAY";
    private static final String KEY_TIME_STAMP_HOURS = "KEY_TIME_STAMP_HOURS";
    private static final String KEY_TIME_STAMP_MINUTES = "KEY_TIME_STAMP_MINUTES";
    private static final String KEY_TIME_STAMP_SECONDS = "KEY_TIME_STAMP_SECONDS";
    private static final String KEY_PULSE_RATE = "KEY_PULSE_RATE";
    private static final String KEY_USER_ID = "KEY_USER_ID";
    private static final String KEY_BODY_MOVEMENT_DETECTION = "KEY_BODY_MOVEMENT_DETECTION";
    private static final String KEY_CUFF_FIT_DETECTION = "KEY_CUFF_FIT_DETECTION";
    private static final String KEY_IRREGULAR_PULSE_DETECTION = "KEY_IRREGULAR_PULSE_DETECTION";
    private static final String KEY_PULSE_RATE_RANGE_DETECTION = "KEY_PULSE_RATE_RANGE_DETECTION";
    private static final String KEY_MEASUREMENT_POSITION_DETECTION = "KEY_MEASUREMENT_POSITION_DETECTION";
    private static final String KEY_NOTIFICATION_COUNT = "KEY_NOTIFICATION_COUNT";
    private static final String KEY_CLIENT_CHARACTERISTIC_CONFIGURATION = "KEY_CLIENT_CHARACTERISTIC_CONFIGURATION";

    private final SavedStateHandle mSavedStateHandle;

    private final MutableLiveData<Boolean> mIsMmhg;

    private final MutableLiveData<Boolean> mIsTimeStampSupported;
    private final MutableLiveData<Boolean> mIsPulseRateSupported;
    private final MutableLiveData<Boolean> mIsUserIdSupported;
    private final MutableLiveData<Boolean> mIsMeasurementStatusSupported;

    private final MutableLiveData<String> mCurrentCuffPressure;
    private final MutableLiveData<String> mTimeStampYear;
    private final MutableLiveData<Integer> mTimeStampMonth;
    private final MutableLiveData<Integer> mTimeStampDay;
    private final MutableLiveData<Integer> mTimeStampHours;
    private final MutableLiveData<Integer> mTimeStampMinutes;
    private final MutableLiveData<Integer> mTimeStampSeconds;
    private final MutableLiveData<String> mPulseRate;
    private final MutableLiveData<String> mUserId;
    private final MutableLiveData<Integer> mBodyMovementDetection;
    private final MutableLiveData<Integer> mCuffFitDetection;
    private final MutableLiveData<Integer> mIrregularPulseDetection;
    private final MutableLiveData<Integer> mPulseRateRangeDetection;
    private final MutableLiveData<Integer> mMeasurementPositionDetection;

    private final MutableLiveData<String> mNotificationCount;

    private final MutableLiveData<String> mClientCharacteristicConfigurationJson;

    @Inject
    public IntermediateCuffPressureSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceSettingRepository deviceSettingRepository, @NonNull Gson gson) {
        super(deviceSettingRepository, gson);
        mSavedStateHandle = savedStateHandle;

        mIsMmhg = savedStateHandle.getLiveData(KEY_IS_MMHG);
        mIsTimeStampSupported = savedStateHandle.getLiveData(KEY_IS_TIME_STAMP_SUPPORTED);
        mIsPulseRateSupported = savedStateHandle.getLiveData(KEY_IS_PULSE_RATE_SUPPORTED);
        mIsUserIdSupported = savedStateHandle.getLiveData(KEY_IS_USER_ID_SUPPORTED);
        mIsMeasurementStatusSupported = savedStateHandle.getLiveData(KEY_IS_MEASUREMENT_STATUS_SUPPORTED);

        mCurrentCuffPressure = savedStateHandle.getLiveData(KEY_CURRENT_CUFF_PRESSURE);
        mTimeStampYear = savedStateHandle.getLiveData(KEY_TIME_STAMP_YEAR);
        mTimeStampMonth = savedStateHandle.getLiveData(KEY_TIME_STAMP_MONTH);
        mTimeStampDay = savedStateHandle.getLiveData(KEY_TIME_STAMP_DAY);
        mTimeStampHours = savedStateHandle.getLiveData(KEY_TIME_STAMP_HOURS);
        mTimeStampMinutes = savedStateHandle.getLiveData(KEY_TIME_STAMP_MINUTES);
        mTimeStampSeconds = savedStateHandle.getLiveData(KEY_TIME_STAMP_SECONDS);
        mPulseRate = savedStateHandle.getLiveData(KEY_PULSE_RATE);
        mUserId = savedStateHandle.getLiveData(KEY_USER_ID);
        mBodyMovementDetection = savedStateHandle.getLiveData(KEY_BODY_MOVEMENT_DETECTION);
        mCuffFitDetection = savedStateHandle.getLiveData(KEY_CUFF_FIT_DETECTION);
        mIrregularPulseDetection = savedStateHandle.getLiveData(KEY_IRREGULAR_PULSE_DETECTION);
        mPulseRateRangeDetection = savedStateHandle.getLiveData(KEY_PULSE_RATE_RANGE_DETECTION);
        mMeasurementPositionDetection = savedStateHandle.getLiveData(KEY_MEASUREMENT_POSITION_DETECTION);

        mNotificationCount = savedStateHandle.getLiveData(KEY_NOTIFICATION_COUNT);

        mClientCharacteristicConfigurationJson = savedStateHandle.getLiveData(KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON);
    }

    @Override
    public void observeSetup(@NonNull Intent intent
            , @NonNull Action onComplete
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mCharacteristicData == null) {
                try {
                    mCharacteristicData = mGson.fromJson(intent.getStringExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString())
                            , CharacteristicData.class);
                } catch (JsonSyntaxException e) {
                    e.printStackTrace();
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

                boolean isMmhg;
                if (intermediateCuffPressure == null) {
                    isMmhg = true;
                } else {
                    isMmhg = BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags());
                }
                mIsMmhg.postValue(isMmhg);


                if (mIsMmhg.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mIsMmhg.postValue(true);
                    } else {
                        mIsMmhg.postValue(BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(intermediateCuffPressure.getFlags()));
                    }
                }

                if (mCurrentCuffPressure.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mCurrentCuffPressure.postValue(null);
                    } else {
                        if (isMmhg) {
                            mCurrentCuffPressure.postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureMmhg().getSfloat()));
                        } else {
                            mCurrentCuffPressure.postValue(String.valueOf(intermediateCuffPressure.getIntermediateCuffPressureCompoundValueCurrentCuffPressureKpa().getSfloat()));
                        }
                    }
                }

                if (mIsTimeStampSupported.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mIsTimeStampSupported.postValue(false);
                    } else {
                        mIsTimeStampSupported.postValue(BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags()));
                    }
                }

                if (mTimeStampYear.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mTimeStampYear.postValue(null);
                    } else {
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            mTimeStampYear.postValue(String.valueOf(intermediateCuffPressure.getYear()));
                        }
                    }
                }

                if (mTimeStampMonth.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            List<Pair<Integer, String>> list = provideDateTimeMonthList();
                            Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair
                                    -> integerStringPair.first == intermediateCuffPressure.getMonth()).findFirst();
                            optional.ifPresent(integerStringPair -> mTimeStampMonth.postValue(list.indexOf(integerStringPair)));
                        }
                    }
                }

                if (mTimeStampDay.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            List<Pair<Integer, String>> list = provideDateTimeDayList();
                            Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair
                                    -> integerStringPair.first == intermediateCuffPressure.getDay()).findFirst();
                            optional.ifPresent(integerStringPair -> mTimeStampDay.postValue(list.indexOf(integerStringPair)));
                        }
                    }
                }

                if (mTimeStampHours.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            mTimeStampHours.postValue(provideDateTimeHoursList().indexOf(String.valueOf(intermediateCuffPressure.getHours())));
                        }
                    }
                }

                if (mTimeStampMinutes.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            mTimeStampMinutes.postValue(provideDateTimeMinutesList().indexOf(String.valueOf(intermediateCuffPressure.getMinutes())));
                        }
                    }
                }

                if (mTimeStampSeconds.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsTimeStampPresent(intermediateCuffPressure.getFlags())) {
                            mTimeStampSeconds.postValue(provideDateTimeSecondsList().indexOf(String.valueOf(intermediateCuffPressure.getSeconds())));
                        }
                    }
                }

                if (mIsPulseRateSupported.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mIsPulseRateSupported.postValue(false);
                    } else {
                        mIsPulseRateSupported.postValue(BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags()));
                    }
                }

                if (mPulseRate.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mPulseRate.postValue(null);
                    } else {
                        if (BloodPressureMeasurementUtils.isFlagsPulseRatePresent(intermediateCuffPressure.getFlags())) {
                            mPulseRate.postValue(String.valueOf(intermediateCuffPressure.getPulseRate().getSfloat()));
                        }
                    }
                }

                if (mIsUserIdSupported.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mIsUserIdSupported.postValue(false);
                    } else {
                        mIsUserIdSupported.postValue(BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags()));
                    }
                }

                if (mUserId.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mUserId.postValue(null);
                    } else {
                        if (BloodPressureMeasurementUtils.isFlagsUserIdPresent(intermediateCuffPressure.getFlags())) {
                            mUserId.postValue(String.valueOf(intermediateCuffPressure.getUserId()));
                        }
                    }
                }

                if (mIsMeasurementStatusSupported.getValue() == null) {
                    if (intermediateCuffPressure == null) {
                        mIsMeasurementStatusSupported.postValue(false);
                    } else {
                        mIsMeasurementStatusSupported.postValue(BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags()));
                    }
                }

                int measurementStatus;
                if (intermediateCuffPressure == null) {
                    measurementStatus = 0;
                } else {
                    if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                        measurementStatus = BLEUtils.createUInt16(intermediateCuffPressure.getMeasurementStatus(), 0);
                    } else {
                        measurementStatus = 0;
                    }
                }

                if (mBodyMovementDetection.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                            List<Pair<Integer, String>> list = provideBodyMovementDetectionList();
                            int maskedBodyMovementDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == maskedBodyMovementDetection).findFirst();
                            optional.ifPresent(integerStringPair -> mBodyMovementDetection.postValue(list.indexOf(integerStringPair)));
                        }
                    }
                }

                if (mCuffFitDetection.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                            List<Pair<Integer, String>> list = provideCuffFitDetectionList();
                            int maskedCuffFitDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == maskedCuffFitDetection).findFirst();
                            optional.ifPresent(integerStringPair -> mCuffFitDetection.postValue(list.indexOf(integerStringPair)));
                        }
                    }
                }

                if (mIrregularPulseDetection.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                            List<Pair<Integer, String>> list = provideIrregularPulseDetectionList();
                            int maskedIrregularPulseDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == maskedIrregularPulseDetection).findFirst();
                            optional.ifPresent(integerStringPair -> mIrregularPulseDetection.postValue(list.indexOf(integerStringPair)));
                        }
                    }
                }

                if (mPulseRateRangeDetection.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                            List<Pair<Integer, String>> list = providePulseRateRangeDetectionList();
                            int maskedPulseRateRangeDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == maskedPulseRateRangeDetection).findFirst();
                            optional.ifPresent(integerStringPair -> mPulseRateRangeDetection.postValue(list.indexOf(integerStringPair)));
                        }
                    }
                }

                if (mMeasurementPositionDetection.getValue() == null) {
                    if (intermediateCuffPressure != null) {
                        if (BloodPressureMeasurementUtils.isFlagsMeasurementStatusPresent(intermediateCuffPressure.getFlags())) {
                            List<Pair<Integer, String>> list = provideMeasurementPositionDetectionList();
                            int maskedMeasurementPositionDetection = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK & measurementStatus;
                            Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == maskedMeasurementPositionDetection).findFirst();
                            optional.ifPresent(integerStringPair -> mMeasurementPositionDetection.postValue(list.indexOf(integerStringPair)));
                        }
                    }
                }

                if (mClientCharacteristicConfigurationJson.getValue() == null) {
                    Optional<DescriptorData> clientCharacteristicConfigurationOptional = mCharacteristicData.descriptorDataList
                            .stream()
                            .filter(descriptorData -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR))
                            .findAny();
                    if (clientCharacteristicConfigurationOptional.isPresent()) {
                        DescriptorData descriptorData = clientCharacteristicConfigurationOptional.get();
                        mClientCharacteristicConfigurationJson.postValue(mGson.toJson(descriptorData));
                        if (descriptorData.data != null) {
                            mSavedStateHandle.<String>getLiveData(KEY_CLIENT_CHARACTERISTIC_CONFIGURATION)
                                    .postValue(mDeviceSettingRepository.getNotificationsString(new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesIndicationsDisabled()));
                        }
                    }
                }

                if (mNotificationCount.getValue() == null) {
                    mNotificationCount.postValue(String.valueOf(mCharacteristicData.notificationCount));
                }

                emitter.onComplete();
            } else {
                emitter.onError(new RuntimeException("Initialized"));
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeIsMmhg(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsMmhg).observe(owner, observer);
    }

    @MainThread
    public void observeUnit(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIsMmhg).observe(owner
                , isMmhg -> observer.onChanged(mDeviceSettingRepository.getUnitString(isMmhg)));
    }

    @MainThread
    public void observeCurrentCuffPressure(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mCurrentCuffPressure).observe(owner, observer);
    }

    @MainThread
    public void observeCurrentCuffPressureError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mCurrentCuffPressure).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getCurrentCuffPressureErrorString(s)));
    }

    @MainThread
    public void observeIsTimeStampSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsTimeStampSupported).observe(owner, observer);
    }

    @MainThread
    public void observeTimeStampYear(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mTimeStampYear).observe(owner, observer);
    }

    @MainThread
    public void observeTimeStampYearError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mTimeStampYear).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getDateTimeYearErrorString(s)));
    }

    @MainThread
    public void observeTimeStampMonth(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mTimeStampMonth).observe(owner, new MapObserver<>(index -> provideDateTimeMonthList().get(index).second, observer));
    }

    @MainThread
    public void observeTimeStampDay(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mTimeStampDay).observe(owner, new MapObserver<>(index -> provideDateTimeDayList().get(index).second, observer));
    }

    @MainThread
    public void observeTimeStampHours(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mTimeStampHours).observe(owner, new MapObserver<>(index -> provideDateTimeHoursList().get(index), observer));
    }

    @MainThread
    public void observeTimeStampMinutes(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mTimeStampMinutes).observe(owner, new MapObserver<>(index -> provideDateTimeMinutesList().get(index), observer));
    }

    @MainThread
    public void observeTimeStampSeconds(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mTimeStampSeconds).observe(owner, new MapObserver<>(index -> provideDateTimeSecondsList().get(index), observer));
    }

    @MainThread
    public void observeIsPulseRateSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsPulseRateSupported).observe(owner, observer);
    }

    @MainThread
    public void observePulseRate(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mPulseRate).observe(owner, observer);
    }

    @MainThread
    public void observePulseRateError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mPulseRate).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getPulseRateErrorString(s)));
    }

    @MainThread
    public void observeIsUserIdSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsUserIdSupported).observe(owner, observer);
    }

    @MainThread
    public void observeUserId(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mUserId).observe(owner, observer);
    }

    @MainThread
    public void observeUserIdError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mUserId).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getUserIdErrorString(s)));
    }

    @MainThread
    public void observeIsMeasurementStatusSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsMeasurementStatusSupported).observe(owner, observer);
    }

    @MainThread
    public void observeBodyMovementDetection(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mBodyMovementDetection).observe(owner, new MapObserver<>(index -> provideBodyMovementDetectionList().get(index).second, observer));
    }

    @MainThread
    public void observeCuffFitDetection(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mCuffFitDetection).observe(owner, new MapObserver<>(index -> provideCuffFitDetectionList().get(index).second, observer));
    }

    @MainThread
    public void observeIrregularPulseDetection(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mIrregularPulseDetection).observe(owner, new MapObserver<>(index -> provideIrregularPulseDetectionList().get(index).second, observer));
    }

    @MainThread
    public void observePulseRateRangeDetection(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mPulseRateRangeDetection).observe(owner, new MapObserver<>(index -> providePulseRateRangeDetectionList().get(index).second, observer));
    }

    @MainThread
    public void observeMeasurementPositionDetection(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mMeasurementPositionDetection).observe(owner, new MapObserver<>(index -> provideMeasurementPositionDetectionList().get(index).second, observer));
    }

    @MainThread
    public void observeHasClientCharacteristicConfigurationData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mClientCharacteristicConfigurationJson).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeClientCharacteristicConfiguration(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_CLIENT_CHARACTERISTIC_CONFIGURATION)).observe(owner, observer);
    }

    @MainThread
    public void observeNotificationCount(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mNotificationCount).observe(owner, observer);
    }

    @MainThread
    public void observeNotificationCountError(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mNotificationCount).observe(owner
                , s -> observer.onChanged(mDeviceSettingRepository.getIndicationCountErrorString(s)));
    }

    @MainThread
    public void updateIsMmhg(@NonNull Boolean isMmhg) {
        mIsMmhg.setValue(isMmhg);
    }

    @MainThread
    public void updateCurrentCuffPressure(@NonNull String text) {
        mCurrentCuffPressure.setValue(text);
    }

    @MainThread
    public void updateIsTimeStampSupported(Boolean checked) {
        mIsTimeStampSupported.setValue(checked);
    }

    @MainThread
    public void updateTimeStampYear(@NonNull String text) {
        mTimeStampYear.setValue(text);
    }

    @MainThread
    public void updateTimeStampMonth(int index) {
        mTimeStampMonth.postValue(index);
    }

    @MainThread
    public void updateTimeStampDay(int index) {
        mTimeStampDay.setValue(index);
    }

    @MainThread
    public void updateTimeStampHours(int index) {
        mTimeStampHours.setValue(index);
    }

    @MainThread
    public void updateTimeStampMinutes(int index) {
        mTimeStampMinutes.setValue(index);
    }

    @MainThread
    public void updateTimeStampSeconds(int index) {
        mTimeStampSeconds.setValue(index);
    }

    @MainThread
    public void updateIsPulseRateSupported(Boolean checked) {
        mIsPulseRateSupported.setValue(checked);
    }

    @MainThread
    public void updatePulseRate(@NonNull String text) {
        mPulseRate.setValue(text);
    }

    @MainThread
    public void updateIsUserIdSupported(Boolean checked) {
        mIsUserIdSupported.setValue(checked);
    }

    @MainThread
    public void updateUserId(@NonNull String text) {
        mUserId.setValue(text);
    }

    @MainThread
    public void updateIsMeasurementStatusSupported(Boolean checked) {
        mIsMeasurementStatusSupported.setValue(checked);
    }

    @MainThread
    public void updateBodyMovementDetection(int index) {
        mBodyMovementDetection.setValue(index);
    }

    @MainThread
    public void updateCuffFitDetection(int index) {
        mCuffFitDetection.setValue(index);
    }

    @MainThread
    public void updateIrregularPulseDetection(int index) {
        mIrregularPulseDetection.setValue(index);
    }

    @MainThread
    public void updatePulseRateRangeDetection(int index) {
        mPulseRateRangeDetection.setValue(index);
    }

    @MainThread
    public void updateMeasurementPositionDetection(int index) {
        mMeasurementPositionDetection.setValue(index);
    }

    @MainThread
    public void updateNotificationCount(@NonNull String text) {
        mNotificationCount.setValue(text);
    }

    @MainThread
    public void setClientCharacteristicConfigurationDescriptorJson(@Nullable String clientCharacteristicConfigurationJson) {
        mClientCharacteristicConfigurationJson.setValue(clientCharacteristicConfigurationJson);
        MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_CLIENT_CHARACTERISTIC_CONFIGURATION);
        if (clientCharacteristicConfigurationJson == null) {
            liveData.setValue(null);
        } else {
            try {
                DescriptorData descriptorData = mGson.fromJson(clientCharacteristicConfigurationJson, DescriptorData.class);
                if (descriptorData.data == null) {
                    liveData.setValue(null);
                } else {
                    liveData.postValue(mDeviceSettingRepository.getNotificationsString(new ClientCharacteristicConfiguration(descriptorData.data).isPropertiesIndicationsDisabled()));
                }
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    public String getClientCharacteristicConfigurationDescriptorJson() {
        return mClientCharacteristicConfigurationJson.getValue();
    }

    @NonNull
    public List<Pair<Integer, String>> provideDateTimeMonthList() {
        return mDeviceSettingRepository.provideDateTimeMonthList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideDateTimeDayList() {
        return mDeviceSettingRepository.provideDateTimeDayList();
    }

    @NonNull
    public List<String> provideDateTimeHoursList() {
        return mDeviceSettingRepository.provideDateTimeHoursList();
    }

    @NonNull
    public List<String> provideDateTimeMinutesList() {
        return mDeviceSettingRepository.provideDateTimeMinutesList();
    }

    @NonNull
    public List<String> provideDateTimeSecondsList() {
        return mDeviceSettingRepository.provideDateTimeSecondsList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideBodyMovementDetectionList() {
        return mDeviceSettingRepository.provideBodyMovementDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideCuffFitDetectionList() {
        return mDeviceSettingRepository.provideCuffFitDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideIrregularPulseDetectionList() {
        return mDeviceSettingRepository.provideIrregularPulseDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> providePulseRateRangeDetectionList() {
        return mDeviceSettingRepository.providePulseRateRangeDetectionList();
    }

    @NonNull
    public List<Pair<Integer, String>> provideMeasurementPositionDetectionList() {
        return mDeviceSettingRepository.provideMeasurementPositionDetectionList();
    }

    @Override
    public void observeSave(@NonNull Consumer<Intent> onSuccess
            , @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Single.<Intent>create(emitter -> {
            CharacteristicData characteristicData = mCharacteristicData;
            if (characteristicData == null) {
                emitter.onError(new RuntimeException("Already saved"));
            } else {
                boolean isMmhg = Boolean.TRUE.equals(mIsMmhg.getValue());
                String currentCuffPressure = mCurrentCuffPressure.getValue();
                boolean isTimeStampSupported = Boolean.TRUE.equals(mIsTimeStampSupported.getValue());
                String year = mTimeStampYear.getValue();
                Integer month = mTimeStampMonth.getValue();
                Integer day = mTimeStampDay.getValue();
                Integer hours = mTimeStampHours.getValue();
                Integer minutes = mTimeStampMinutes.getValue();
                Integer seconds = mTimeStampSeconds.getValue();
                boolean isPulseRateSupported = Boolean.TRUE.equals(mIsPulseRateSupported.getValue());
                String pulseRate = mPulseRate.getValue();
                boolean isUserIdSupported = Boolean.TRUE.equals(mIsUserIdSupported.getValue());
                String userId = mUserId.getValue();
                boolean isMeasurementStatusSupported = Boolean.TRUE.equals(mIsMeasurementStatusSupported.getValue());
                Integer bodyMovementDetection = mBodyMovementDetection.getValue();
                Integer cuffFitDetection = mCuffFitDetection.getValue();
                Integer irregularPulseDetection = mIrregularPulseDetection.getValue();
                Integer pulseRateRangeDetection = mPulseRateRangeDetection.getValue();
                Integer measurementPositionDetection = mMeasurementPositionDetection.getValue();
                String indicationCount = mNotificationCount.getValue();
                String clientCharacteristicConfigurationJson = mClientCharacteristicConfigurationJson.getValue();

                if (currentCuffPressure != null && mDeviceSettingRepository.getCurrentCuffPressureErrorString(currentCuffPressure) == null
                        && (!isTimeStampSupported || (year != null && mDeviceSettingRepository.getDateTimeYearErrorString(year) == null))
                        && (!isPulseRateSupported || (pulseRate != null && mDeviceSettingRepository.getPulseRateErrorString(pulseRate) == null))
                        && (!isUserIdSupported || (userId != null && mDeviceSettingRepository.getUserIdErrorString(userId) == null))
                        && (!isMeasurementStatusSupported || (bodyMovementDetection != null && cuffFitDetection != null && irregularPulseDetection != null && pulseRateRangeDetection != null && measurementPositionDetection != null))
                        && indicationCount != null && mDeviceSettingRepository.getIndicationCountErrorString(indicationCount) == null
                        && clientCharacteristicConfigurationJson != null) {
                    int flags = 0;
                    if (!isMmhg) {
                        flags |= BloodPressureMeasurementUtils.FLAG_BLOOD_PRESSURE_UNITS_KPA;
                    }
                    if (isTimeStampSupported) {
                        flags |= BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT;
                    }
                    if (isPulseRateSupported) {
                        flags |= BloodPressureMeasurementUtils.FLAG_PULSE_RATE_PRESENT;
                    }
                    if (isUserIdSupported) {
                        flags |= BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT;
                    }
                    if (isMeasurementStatusSupported) {
                        flags |= BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
                    }

                    IEEE_11073_20601_SFLOAT currentCuffPressureMmhg;
                    IEEE_11073_20601_SFLOAT currentCuffPressureKpa;
                    if (BloodPressureMeasurementUtils.isFlagsBloodPressureUnitsMmhg(flags)) {
                        currentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(Double.parseDouble(currentCuffPressure));

                        currentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
                    } else {
                        currentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);

                        currentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(Double.parseDouble(currentCuffPressure));
                    }

                    if (Boolean.TRUE.equals(isTimeStampSupported)) {
                        month = provideDateTimeMonthList().get(Objects.requireNonNull(month)).first;
                        day = provideDateTimeDayList().get(Objects.requireNonNull(day)).first;
                        hours = Integer.parseInt(provideDateTimeHoursList().get(Objects.requireNonNull(hours)));
                        minutes = Integer.parseInt(provideDateTimeMinutesList().get(Objects.requireNonNull(minutes)));
                        seconds = Integer.parseInt(provideDateTimeSecondsList().get(Objects.requireNonNull(seconds)));
                    } else {
                        year = String.valueOf(DateTimeUtils.YEAR_IS_NOT_KNOWN);
                        month = DateTimeUtils.MONTH_IS_NOT_KNOWN;
                        day = DateTimeUtils.DAY_OF_MONTH_IS_NOT_KNOWN;
                        hours = 0;
                        minutes = 0;
                        seconds = 0;
                    }

                    IEEE_11073_20601_SFLOAT pulseRateSfloat;
                    if (Boolean.TRUE.equals(isPulseRateSupported)) {
                        pulseRateSfloat = new IEEE_11073_20601_SFLOAT(Double.parseDouble(Objects.requireNonNull(pulseRate)));
                    } else {
                        pulseRateSfloat = new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN);
                    }

                    if (!isUserIdSupported) {
                        userId = String.valueOf(UserIndexUtils.USER_ID_UNKNOWN_USER);
                    }

                    byte[] measurementStatus;
                    if (Boolean.TRUE.equals(isMeasurementStatusSupported)) {
                        int measurementStatusFlags = provideBodyMovementDetectionList().get(Objects.requireNonNull(bodyMovementDetection)).first;
                        measurementStatusFlags |= provideCuffFitDetectionList().get(Objects.requireNonNull(cuffFitDetection)).first;
                        measurementStatusFlags |= provideIrregularPulseDetectionList().get(Objects.requireNonNull(irregularPulseDetection)).first;
                        measurementStatusFlags |= providePulseRateRangeDetectionList().get(Objects.requireNonNull(pulseRateRangeDetection)).first;
                        measurementStatusFlags |= provideMeasurementPositionDetectionList().get(Objects.requireNonNull(measurementPositionDetection)).first;

                        measurementStatus = new byte[]{(byte) measurementStatusFlags, (byte) (measurementStatusFlags >> 8)};
                    } else {
                        measurementStatus = new byte[0];
                    }

                    characteristicData.data = new IntermediateCuffPressure(flags
                            , currentCuffPressureMmhg
                            , currentCuffPressureKpa
                            , new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN)
                            , new IEEE_11073_20601_SFLOAT(BLEUtils.SFLOAT_NAN)
                            , Integer.parseInt(year)
                            , month
                            , day
                            , hours
                            , minutes
                            , seconds
                            , pulseRateSfloat
                            , Integer.parseInt(userId)
                            , measurementStatus).getBytes();

                    mCharacteristicData.descriptorDataList.clear();
                    mCharacteristicData.descriptorDataList.add(mGson.fromJson(mClientCharacteristicConfigurationJson.getValue(), DescriptorData.class));
                    mCharacteristicData.notificationCount = Integer.parseInt(indicationCount);

                    Intent intent = new Intent();
                    intent.putExtra(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC.toString(), mGson.toJson(characteristicData));

                    mCharacteristicData = null;
                    emitter.onSuccess(intent);
                } else {
                    emitter.onError(new RuntimeException("Validation failed"));
                }
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onSuccess, onError));
    }

}