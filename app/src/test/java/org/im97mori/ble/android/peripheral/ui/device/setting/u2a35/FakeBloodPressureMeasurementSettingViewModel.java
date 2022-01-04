package org.im97mori.ble.android.peripheral.ui.device.setting.u2a35;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.processors.PublishProcessor;

@HiltViewModel
public class FakeBloodPressureMeasurementSettingViewModel extends BloodPressureMeasurementSettingViewModel {

    public final PublishProcessor<String> mObserveSetupProcessor = PublishProcessor.create();

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private final SavedStateHandle mSavedStateHandle;

    public final PublishProcessor<Intent> mObserveSaveProcessor = PublishProcessor.create();

    public java.util.function.Consumer<Boolean> mUpdateIsMmhgConsumer;
    public java.util.function.Consumer<String> mUpdateSystolicConsumer;
    public java.util.function.Consumer<String> mUpdateDiastolicConsumer;
    public java.util.function.Consumer<String> mUpdateMeanArterialPressureConsumer;
    public java.util.function.Consumer<Boolean> mUpdateIsTimeStampSupportedConsumer;
    public java.util.function.Consumer<String> mUpdateTimeStampYearConsumer;
    public java.util.function.Consumer<Integer> mUpdateTimeStampMonthConsumer;
    public java.util.function.Consumer<Integer> mUpdateTimeStampDayConsumer;
    public java.util.function.Consumer<Integer> mUpdateTimeStampHoursConsumer;
    public java.util.function.Consumer<Integer> mUpdateTimeStampMinutesConsumer;
    public java.util.function.Consumer<Integer> mUpdateTimeStampSecondsConsumer;
    public java.util.function.Consumer<Boolean> mUpdateIsPulseRateSupportedConsumer;
    public java.util.function.Consumer<String> mUpdatePulseRateConsumer;
    public java.util.function.Consumer<Boolean> mUpdateIsUserIdSupportedConsumer;
    public java.util.function.Consumer<String> mUpdateUserIdConsumer;
    public java.util.function.Consumer<Boolean> mUpdateIsMeasurementStatusSupportedConsumer;
    public java.util.function.Consumer<Integer> mUpdateBodyMovementDetectionConsumer;
    public java.util.function.Consumer<Integer> mUpdateCuffFitDetectionConsumer;
    public java.util.function.Consumer<Integer> mUpdateIrregularPulseDetectionConsumer;
    public java.util.function.Consumer<Integer> mUpdatePulseRateRangeDetectionConsumer;
    public java.util.function.Consumer<Integer> mUpdateMeasurementPositionDetectionConsumer;
    public java.util.function.Consumer<String> mUpdateIndicationCountConsumer;

    @Inject
    FakeBloodPressureMeasurementSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, gson);
        mSavedStateHandle = savedStateHandle;
        mFakeDeviceSettingRepository = deviceSettingRepository;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSetupProcessor
                .subscribe(s -> mDisposable.add(Single.<String>create(emitter -> emitter.onSuccess(s))
                        .flatMapCompletable(t -> {
                            switch (t) {
                                case "test_systolic_error_00002": test_systolic_error_00002(); break;
                                case "test_diastolic_error_00002": test_diastolic_error_00002(); break;
                                case "test_meanArterialPressure_error_00002": test_meanArterialPressure_error_00002(); break;
                                case "test_isTimeStampSupported_00002": test_isTimeStampSupported_00002(); break;
                                case "test_isTimeStampSupported_00003": test_isTimeStampSupported_00003(); break;
                                case "test_timeStampYear_visibility_00001": test_timeStampYear_visibility_00001(); break;
                                case "test_timeStampYear_visibility_00002": test_timeStampYear_visibility_00002(); break;
                                case "test_timeStampYear_error_00002": test_timeStampYear_error_00002(); break;
                                case "test_timeStampMonth_visibility_00001": test_timeStampMonth_visibility_00001(); break;
                                case "test_timeStampMonth_visibility_00002": test_timeStampMonth_visibility_00002(); break;
                                case "test_timeStampDay_visibility_00001": test_timeStampDay_visibility_00001(); break;
                                case "test_timeStampDay_visibility_00002": test_timeStampDay_visibility_00002(); break;
                                case "test_timeStampHours_visibility_00001": test_timeStampHours_visibility_00001(); break;
                                case "test_timeStampHours_visibility_00002": test_timeStampHours_visibility_00002(); break;
                                case "test_timeStampMinutes_visibility_00001": test_timeStampMinutes_visibility_00001(); break;
                                case "test_timeStampMinutes_visibility_00002": test_timeStampMinutes_visibility_00002(); break;
                                case "test_timeStampSeconds_visibility_00001": test_timeStampSeconds_visibility_00001(); break;
                                case "test_timeStampSeconds_visibility_00002": test_timeStampSeconds_visibility_00002(); break;
                                case "test_isPulseRateSupported_00002": test_isPulseRateSupported_00002(); break;
                                case "test_isPulseRateSupported_00003": test_isPulseRateSupported_00003(); break;
                                case "test_pulseRate_visibility_00001": test_pulseRate_visibility_00001(); break;
                                case "test_pulseRate_visibility_00002": test_pulseRate_visibility_00002(); break;
                                case "test_pulseRate_error_00002": test_pulseRate_error_00002(); break;
                                case "test_isUserIdSupported_00002": test_isUserIdSupported_00002(); break;
                                case "test_isUserIdSupported_00003": test_isUserIdSupported_00003(); break;
                                case "test_userId_visibility_00001": test_userId_visibility_00001(); break;
                                case "test_userId_visibility_00002": test_userId_visibility_00002(); break;
                                case "test_userId_error_00002": test_userId_error_00002(); break;
                                case "test_isMeasurementStatusSupported_00002": test_isMeasurementStatusSupported_00002(); break;
                                case "test_isMeasurementStatusSupported_00003": test_isMeasurementStatusSupported_00003(); break;
                                case "test_bodyMovementDetection_visibility_00001": test_bodyMovementDetection_visibility_00001(); break;
                                case "test_bodyMovementDetection_visibility_00002": test_bodyMovementDetection_visibility_00002(); break;
                                case "test_cuffFitDetection_visibility_00001": test_cuffFitDetection_visibility_00001(); break;
                                case "test_cuffFitDetection_visibility_00002": test_cuffFitDetection_visibility_00002(); break;
                                case "test_irregularPulseDetection_visibility_00001": test_irregularPulseDetection_visibility_00001(); break;
                                case "test_irregularPulseDetection_visibility_00002": test_irregularPulseDetection_visibility_00002(); break;
                                case "test_pulseRateRangeDetection_visibility_00001": test_pulseRateRangeDetection_visibility_00001(); break;
                                case "test_pulseRateRangeDetection_visibility_00002": test_pulseRateRangeDetection_visibility_00002(); break;
                                case "test_measurementPositionDetection_visibility_00001": test_measurementPositionDetection_visibility_00001(); break;
                                case "test_measurementPositionDetection_visibility_00002": test_measurementPositionDetection_visibility_00002(); break;
                                case "test_clientCharacteristicConfigurationCardView_00002": test_clientCharacteristicConfigurationCardView_00002(); break;
                                case "test_clientCharacteristicConfigurationSettingButton_00002": test_clientCharacteristicConfigurationSettingButton_00002(); break;
                                case "test_indicationCount_error_00002": test_indicationCount_error_00002(); break;
                                default:
                            }
                            return Completable.complete();
                        }).subscribe(onComplete, onError))));
    }

    @Override
    public void updateIsMmhg(boolean isMmhg) {
        if (mUpdateIsMmhgConsumer != null) {
            mUpdateIsMmhgConsumer.accept(isMmhg);
        }
        super.updateIsMmhg(isMmhg);
    }

    @Override
    public void updateSystolic(@NonNull String text) {
        if (mUpdateSystolicConsumer != null) {
            mUpdateSystolicConsumer.accept(text);
        }
        super.updateSystolic(text);
    }

    @Override
    public void updateDiastolic(@NonNull String text) {
        if (mUpdateDiastolicConsumer != null) {
            mUpdateDiastolicConsumer.accept(text);
        }
        super.updateDiastolic(text);
    }

    @Override
    public void updateMeanArterialPressure(@NonNull String text) {
        if (mUpdateMeanArterialPressureConsumer != null) {
            mUpdateMeanArterialPressureConsumer.accept(text);
        }
        super.updateMeanArterialPressure(text);
    }

    @Override
    public void updateIsTimeStampSupported(boolean checked) {
        if (mUpdateIsTimeStampSupportedConsumer != null) {
            mUpdateIsTimeStampSupportedConsumer.accept(checked);
        }
        super.updateIsTimeStampSupported(checked);
    }

    @Override
    public void updateTimeStampYear(@NonNull String text) {
        if (mUpdateTimeStampYearConsumer != null) {
            mUpdateTimeStampYearConsumer.accept(text);
        }
        super.updateTimeStampYear(text);
    }

    @Override
    public void updateTimeStampMonth(int index) {
        if (mUpdateTimeStampMonthConsumer != null) {
            mUpdateTimeStampMonthConsumer.accept(index);
        }
        super.updateTimeStampMonth(index);
    }

    @Override
    public void updateTimeStampDay(int index) {
        if (mUpdateTimeStampDayConsumer != null) {
            mUpdateTimeStampDayConsumer.accept(index);
        }
        super.updateTimeStampDay(index);
    }

    @Override
    public void updateTimeStampHours(int index) {
        if (mUpdateTimeStampHoursConsumer != null) {
            mUpdateTimeStampHoursConsumer.accept(index);
        }
        super.updateTimeStampHours(index);
    }

    @Override
    public void updateTimeStampMinutes(int index) {
        if (mUpdateTimeStampMinutesConsumer != null) {
            mUpdateTimeStampMinutesConsumer.accept(index);
        }
        super.updateTimeStampMinutes(index);
    }

    @Override
    public void updateTimeStampSeconds(int index) {
        if (mUpdateTimeStampSecondsConsumer != null) {
            mUpdateTimeStampSecondsConsumer.accept(index);
        }
        super.updateTimeStampSeconds(index);
    }

    @Override
    public void updateIsPulseRateSupported(boolean checked) {
        if (mUpdateIsPulseRateSupportedConsumer != null) {
            mUpdateIsPulseRateSupportedConsumer.accept(checked);
        }
        super.updateIsPulseRateSupported(checked);
    }

    @Override
    public void updatePulseRate(@NonNull String text) {
        if (mUpdatePulseRateConsumer != null) {
            mUpdatePulseRateConsumer.accept(text);
        }
        super.updatePulseRate(text);
    }

    @Override
    public void updateIsUserIdSupported(boolean checked) {
        if (mUpdateIsUserIdSupportedConsumer != null) {
            mUpdateIsUserIdSupportedConsumer.accept(checked);
        }
        super.updateIsUserIdSupported(checked);
    }

    @Override
    public void updateUserId(@NonNull String text) {
        if (mUpdateUserIdConsumer != null) {
            mUpdateUserIdConsumer.accept(text);
        }
        super.updateUserId(text);
    }

    @Override
    public void updateIsMeasurementStatusSupported(boolean checked) {
        if (mUpdateIsMeasurementStatusSupportedConsumer != null) {
            mUpdateIsMeasurementStatusSupportedConsumer.accept(checked);
        }
        super.updateIsMeasurementStatusSupported(checked);
    }

    @Override
    public void updateBodyMovementDetection(int index) {
        if (mUpdateBodyMovementDetectionConsumer != null) {
            mUpdateBodyMovementDetectionConsumer.accept(index);
        }
        super.updateBodyMovementDetection(index);
    }

    @Override
    public void updateCuffFitDetection(int index) {
        if (mUpdateCuffFitDetectionConsumer != null) {
            mUpdateCuffFitDetectionConsumer.accept(index);
        }
        super.updateCuffFitDetection(index);
    }

    @Override
    public void updateIrregularPulseDetection(int index) {
        if (mUpdateIrregularPulseDetectionConsumer != null) {
            mUpdateIrregularPulseDetectionConsumer.accept(index);
        }
        super.updateIrregularPulseDetection(index);
    }

    @Override
    public void updatePulseRateRangeDetection(int index) {
        if (mUpdatePulseRateRangeDetectionConsumer != null) {
            mUpdatePulseRateRangeDetectionConsumer.accept(index);
        }
        super.updatePulseRateRangeDetection(index);
    }

    @Override
    public void updateMeasurementPositionDetection(int index) {
        if (mUpdateMeasurementPositionDetectionConsumer != null) {
            mUpdateMeasurementPositionDetectionConsumer.accept(index);
        }
        super.updateMeasurementPositionDetection(index);
    }

    @Override
    public void updateIndicationCount(@NonNull String text) {
        if (mUpdateIndicationCountConsumer != null) {
            mUpdateIndicationCountConsumer.accept(text);
        }
        super.updateIndicationCount(text);
    }

    @Override
    public void observeSave(@NonNull Consumer<Intent> onSuccess, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSaveProcessor.subscribe(onSuccess, onError));
    }

    private void test_systolic_error_00002() {
        mSavedStateHandle.set("KEY_SYSTOLIC", "");
    }

    private void test_diastolic_error_00002() {
        mSavedStateHandle.set("KEY_DIASTOLIC", "");
    }

    private void test_meanArterialPressure_error_00002() {
        mSavedStateHandle.set("KEY_MEAN_ARTERIAL_PRESSURE", "");
    }

    private void test_isTimeStampSupported_00002() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", false);
    }

    private void test_isTimeStampSupported_00003() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", true);
    }

    private void test_timeStampYear_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", false);
    }

    private void test_timeStampYear_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", true);
    }

    private void test_timeStampYear_error_00002() {
        mSavedStateHandle.set("KEY_TIME_STAMP_YEAR", "");
    }

    private void test_timeStampMonth_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", false);
    }

    private void test_timeStampMonth_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", true);
    }

    private void test_timeStampDay_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", false);
    }

    private void test_timeStampDay_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", true);
    }

    private void test_timeStampHours_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", false);
    }

    private void test_timeStampHours_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", true);
    }

    private void test_timeStampMinutes_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", false);
    }

    private void test_timeStampMinutes_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", true);
    }

    private void test_timeStampSeconds_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", false);
    }

    private void test_timeStampSeconds_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", true);
    }

    private void test_isPulseRateSupported_00002() {
        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", false);
    }

    private void test_isPulseRateSupported_00003() {
        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", true);
    }

    private void test_pulseRate_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", false);
    }

    private void test_pulseRate_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", true);
    }

    private void test_pulseRate_error_00002() {
        mSavedStateHandle.set("KEY_PULSE_RATE", "");
    }

    private void test_isUserIdSupported_00002() {
        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", false);
    }

    private void test_isUserIdSupported_00003() {
        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", true);
    }

    private void test_userId_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", false);
    }

    private void test_userId_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", true);
    }

    private void test_userId_error_00002() {
        mSavedStateHandle.set("KEY_USER_ID", "");
    }

    private void test_isMeasurementStatusSupported_00002() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", false);
    }

    private void test_isMeasurementStatusSupported_00003() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", true);
    }

    private void test_bodyMovementDetection_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", false);
    }

    private void test_bodyMovementDetection_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", true);
    }

    private void test_cuffFitDetection_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", false);
    }

    private void test_cuffFitDetection_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", true);
    }

    private void test_irregularPulseDetection_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", false);
    }

    private void test_irregularPulseDetection_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", true);
    }

    private void test_pulseRateRangeDetection_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", false);
    }

    private void test_pulseRateRangeDetection_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", true);
    }

    private void test_measurementPositionDetection_visibility_00001() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", false);
    }

    private void test_measurementPositionDetection_visibility_00002() {
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", true);
    }

    private void test_clientCharacteristicConfigurationCardView_00002() {
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON", "");
    }

    private void test_clientCharacteristicConfigurationSettingButton_00002() {
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON", "a");
    }

    private void test_indicationCount_error_00002() {
        mSavedStateHandle.set("KEY_INDICATION_COUNT", "");
    }

}