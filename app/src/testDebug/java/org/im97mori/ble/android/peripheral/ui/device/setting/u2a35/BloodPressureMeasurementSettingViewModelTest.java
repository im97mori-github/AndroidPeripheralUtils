package org.im97mori.ble.android.peripheral.ui.device.setting.u2a35;

import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK;
import static org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
import static org.im97mori.ble.constants.DescriptorUUID.CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattDescriptor;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.core.util.Pair;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import junit.framework.TestCase;

import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.DescriptorData;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
import org.im97mori.ble.descriptor.u2902.ClientCharacteristicConfiguration;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@SuppressWarnings("ConstantConditions")
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class BloodPressureMeasurementSettingViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private BloodPressureMeasurementSettingViewModel mViewModel;

    private SavedStateHandle mSavedStateHandle;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Gson mGson;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new BloodPressureMeasurementSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository, mGson);
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
        mFakeDeviceSettingRepository = null;
        mSavedStateHandle = null;
    }

    @Test
    public void test_observeSetup_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();
        AtomicReference<String> unitReference = new AtomicReference<>();
        AtomicReference<String> systolicReference = new AtomicReference<>();
        AtomicReference<String> systolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> diastolicReference = new AtomicReference<>();
        AtomicReference<String> diastolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearErrorStringReference = new AtomicReference<>();
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();
        AtomicReference<String> timeStampDayReference = new AtomicReference<>();
        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();
        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();
        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> pulseRateReference = new AtomicReference<>();
        AtomicReference<String> pulseRateErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> userIdReference = new AtomicReference<>();
        AtomicReference<String> userIdErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();
        AtomicReference<String> irregularPulseDetectionReference = new AtomicReference<>();
        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();
        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();
        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();
        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();
        AtomicReference<String> indicationCountReference = new AtomicReference<>();
        AtomicReference<String> indicationCountErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), isMmhgReference::set);
        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);
        mViewModel.observeSystolic(new TestLifeCycleOwner(), systolicReference::set);
        mViewModel.observeSystolicErrorString(new TestLifeCycleOwner(), systolicErrorStringReference::set);
        mViewModel.observeDiastolic(new TestLifeCycleOwner(), diastolicReference::set);
        mViewModel.observeDiastolicErrorString(new TestLifeCycleOwner(), diastolicErrorStringReference::set);
        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), meanArterialPressureReference::set);
        mViewModel.observeMeanArterialPressureErrorString(new TestLifeCycleOwner(), meanArterialPressureErrorStringReference::set);
        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), isTimeStampSupportedReference::set);
        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), timeStampYearReference::set);
        mViewModel.observeTimeStampYearErrorString(new TestLifeCycleOwner(), timeStampYearErrorStringReference::set);
        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), timeStampMonthReference::set);
        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), timeStampDayReference::set);
        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), timeStampHoursReference::set);
        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), timeStampMinutesReference::set);
        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), timeStampSecondsReference::set);
        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), isPulseRateSupportedReference::set);
        mViewModel.observePulseRate(new TestLifeCycleOwner(), pulseRateReference::set);
        mViewModel.observePulseRateErrorString(new TestLifeCycleOwner(), pulseRateErrorStringReference::set);
        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), isUserIdSupportedReference::set);
        mViewModel.observeUserId(new TestLifeCycleOwner(), userIdReference::set);
        mViewModel.observeUserIdErrorString(new TestLifeCycleOwner(), userIdErrorStringReference::set);
        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), isMeasurementStatusSupportedReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetectionReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), irregularPulseDetectionReference::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetectionReference::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetectionReference::set);
        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), hasClientCharacteristicConfigurationDataJsonReference::set);
        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);
        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), indicationCountReference::set);
        mViewModel.observeIndicationCountErrorString(new TestLifeCycleOwner(), indicationCountErrorStringReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isMmhgReference.get());
        assertEquals(mFakeDeviceSettingRepository.getUnitString(isMmhgReference.get()), unitReference.get());
        assertNull(systolicReference.get());
        assertEquals(mFakeDeviceSettingRepository.getSystolicErrorString(systolicReference.get()), systolicErrorStringReference.get());
        assertNull(diastolicReference.get());
        assertEquals(mFakeDeviceSettingRepository.getDiastolicErrorString(diastolicReference.get()), diastolicErrorStringReference.get());
        assertNull(meanArterialPressureReference.get());
        assertEquals(mFakeDeviceSettingRepository.getMeanArterialPressureErrorString(meanArterialPressureReference.get()), meanArterialPressureErrorStringReference.get());
        assertFalse(isTimeStampSupportedReference.get());
        assertNull(timeStampYearReference.get());
        assertEquals(mFakeDeviceSettingRepository.getDateTimeYearErrorString(timeStampYearReference.get()), timeStampYearErrorStringReference.get());
        assertNull(timeStampMonthReference.get());
        assertNull(timeStampDayReference.get());
        assertNull(timeStampHoursReference.get());
        assertNull(timeStampMinutesReference.get());
        assertNull(timeStampSecondsReference.get());
        assertFalse(isPulseRateSupportedReference.get());
        assertNull(pulseRateReference.get());
        assertEquals(mFakeDeviceSettingRepository.getPulseRateErrorString(pulseRateReference.get()), pulseRateErrorStringReference.get());
        assertFalse(isUserIdSupportedReference.get());
        assertNull(userIdReference.get());
        assertEquals(mFakeDeviceSettingRepository.getUserIdErrorString(userIdReference.get()), userIdErrorStringReference.get());
        assertFalse(isMeasurementStatusSupportedReference.get());
        assertNull(bodyMovementDetectionReference.get());
        assertNull(cuffFitDetectionReference.get());
        assertNull(irregularPulseDetectionReference.get());
        assertNull(pulseRateRangeDetectionReference.get());
        assertNull(measurementPositionDetectionReference.get());
        assertNull(hasClientCharacteristicConfigurationDataJsonReference.get());
        assertEquals("", clientCharacteristicConfigurationReference.get());
        assertEquals(-1, Integer.parseInt(indicationCountReference.get()));
        assertNull(indicationCountErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();
        AtomicReference<String> unitReference = new AtomicReference<>();
        AtomicReference<String> systolicReference = new AtomicReference<>();
        AtomicReference<String> systolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> diastolicReference = new AtomicReference<>();
        AtomicReference<String> diastolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearErrorStringReference = new AtomicReference<>();
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();
        AtomicReference<String> timeStampDayReference = new AtomicReference<>();
        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();
        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();
        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> pulseRateReference = new AtomicReference<>();
        AtomicReference<String> pulseRateErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> userIdReference = new AtomicReference<>();
        AtomicReference<String> userIdErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();
        AtomicReference<String> irregularPulseDetectionReference = new AtomicReference<>();
        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();
        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();
        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();
        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();
        AtomicReference<String> indicationCountReference = new AtomicReference<>();
        AtomicReference<String> indicationCountErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), isMmhgReference::set);
        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);
        mViewModel.observeSystolic(new TestLifeCycleOwner(), systolicReference::set);
        mViewModel.observeSystolicErrorString(new TestLifeCycleOwner(), systolicErrorStringReference::set);
        mViewModel.observeDiastolic(new TestLifeCycleOwner(), diastolicReference::set);
        mViewModel.observeDiastolicErrorString(new TestLifeCycleOwner(), diastolicErrorStringReference::set);
        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), meanArterialPressureReference::set);
        mViewModel.observeMeanArterialPressureErrorString(new TestLifeCycleOwner(), meanArterialPressureErrorStringReference::set);
        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), isTimeStampSupportedReference::set);
        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), timeStampYearReference::set);
        mViewModel.observeTimeStampYearErrorString(new TestLifeCycleOwner(), timeStampYearErrorStringReference::set);
        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), timeStampMonthReference::set);
        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), timeStampDayReference::set);
        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), timeStampHoursReference::set);
        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), timeStampMinutesReference::set);
        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), timeStampSecondsReference::set);
        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), isPulseRateSupportedReference::set);
        mViewModel.observePulseRate(new TestLifeCycleOwner(), pulseRateReference::set);
        mViewModel.observePulseRateErrorString(new TestLifeCycleOwner(), pulseRateErrorStringReference::set);
        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), isUserIdSupportedReference::set);
        mViewModel.observeUserId(new TestLifeCycleOwner(), userIdReference::set);
        mViewModel.observeUserIdErrorString(new TestLifeCycleOwner(), userIdErrorStringReference::set);
        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), isMeasurementStatusSupportedReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetectionReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), irregularPulseDetectionReference::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetectionReference::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetectionReference::set);
        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), hasClientCharacteristicConfigurationDataJsonReference::set);
        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);
        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), indicationCountReference::set);
        mViewModel.observeIndicationCountErrorString(new TestLifeCycleOwner(), indicationCountErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData();
        bloodPressureMeasurementCharacteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
        bloodPressureMeasurementCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
        intent.putExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), mGson.toJson(bloodPressureMeasurementCharacteristicData));

        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isMmhgReference.get());
        assertEquals(mFakeDeviceSettingRepository.getUnitString(isMmhgReference.get()), unitReference.get());
        assertNull(systolicReference.get());
        assertEquals(mFakeDeviceSettingRepository.getSystolicErrorString(systolicReference.get()), systolicErrorStringReference.get());
        assertNull(diastolicReference.get());
        assertEquals(mFakeDeviceSettingRepository.getDiastolicErrorString(diastolicReference.get()), diastolicErrorStringReference.get());
        assertNull(meanArterialPressureReference.get());
        assertEquals(mFakeDeviceSettingRepository.getMeanArterialPressureErrorString(meanArterialPressureReference.get()), meanArterialPressureErrorStringReference.get());
        assertFalse(isTimeStampSupportedReference.get());
        assertNull(timeStampYearReference.get());
        assertEquals(mFakeDeviceSettingRepository.getDateTimeYearErrorString(timeStampYearReference.get()), timeStampYearErrorStringReference.get());
        assertNull(timeStampMonthReference.get());
        assertNull(timeStampDayReference.get());
        assertNull(timeStampHoursReference.get());
        assertNull(timeStampMinutesReference.get());
        assertNull(timeStampSecondsReference.get());
        assertFalse(isPulseRateSupportedReference.get());
        assertNull(pulseRateReference.get());
        assertEquals(mFakeDeviceSettingRepository.getPulseRateErrorString(pulseRateReference.get()), pulseRateErrorStringReference.get());
        assertFalse(isUserIdSupportedReference.get());
        assertNull(userIdReference.get());
        assertEquals(mFakeDeviceSettingRepository.getUserIdErrorString(userIdReference.get()), userIdErrorStringReference.get());
        assertFalse(isMeasurementStatusSupportedReference.get());
        assertNull(bodyMovementDetectionReference.get());
        assertNull(cuffFitDetectionReference.get());
        assertNull(irregularPulseDetectionReference.get());
        assertNull(pulseRateRangeDetectionReference.get());
        assertNull(measurementPositionDetectionReference.get());
        assertNull(hasClientCharacteristicConfigurationDataJsonReference.get());
        assertEquals("", clientCharacteristicConfigurationReference.get());
        assertEquals(-1, Integer.parseInt(indicationCountReference.get()));
        assertNull(indicationCountErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();
        AtomicReference<String> unitReference = new AtomicReference<>();
        AtomicReference<String> systolicReference = new AtomicReference<>();
        AtomicReference<String> systolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> diastolicReference = new AtomicReference<>();
        AtomicReference<String> diastolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearErrorStringReference = new AtomicReference<>();
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();
        AtomicReference<String> timeStampDayReference = new AtomicReference<>();
        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();
        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();
        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> pulseRateReference = new AtomicReference<>();
        AtomicReference<String> pulseRateErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> userIdReference = new AtomicReference<>();
        AtomicReference<String> userIdErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();
        AtomicReference<String> irregularPulseDetectionReference = new AtomicReference<>();
        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();
        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();
        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();
        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();
        AtomicReference<String> indicationCountReference = new AtomicReference<>();
        AtomicReference<String> indicationCountErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), isMmhgReference::set);
        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);
        mViewModel.observeSystolic(new TestLifeCycleOwner(), systolicReference::set);
        mViewModel.observeSystolicErrorString(new TestLifeCycleOwner(), systolicErrorStringReference::set);
        mViewModel.observeDiastolic(new TestLifeCycleOwner(), diastolicReference::set);
        mViewModel.observeDiastolicErrorString(new TestLifeCycleOwner(), diastolicErrorStringReference::set);
        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), meanArterialPressureReference::set);
        mViewModel.observeMeanArterialPressureErrorString(new TestLifeCycleOwner(), meanArterialPressureErrorStringReference::set);
        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), isTimeStampSupportedReference::set);
        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), timeStampYearReference::set);
        mViewModel.observeTimeStampYearErrorString(new TestLifeCycleOwner(), timeStampYearErrorStringReference::set);
        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), timeStampMonthReference::set);
        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), timeStampDayReference::set);
        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), timeStampHoursReference::set);
        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), timeStampMinutesReference::set);
        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), timeStampSecondsReference::set);
        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), isPulseRateSupportedReference::set);
        mViewModel.observePulseRate(new TestLifeCycleOwner(), pulseRateReference::set);
        mViewModel.observePulseRateErrorString(new TestLifeCycleOwner(), pulseRateErrorStringReference::set);
        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), isUserIdSupportedReference::set);
        mViewModel.observeUserId(new TestLifeCycleOwner(), userIdReference::set);
        mViewModel.observeUserIdErrorString(new TestLifeCycleOwner(), userIdErrorStringReference::set);
        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), isMeasurementStatusSupportedReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetectionReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), irregularPulseDetectionReference::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetectionReference::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetectionReference::set);
        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), hasClientCharacteristicConfigurationDataJsonReference::set);
        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);
        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), indicationCountReference::set);
        mViewModel.observeIndicationCountErrorString(new TestLifeCycleOwner(), indicationCountErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData();
        bloodPressureMeasurementCharacteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
        bloodPressureMeasurementCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
        int bloodPressureMeasurementFlags = 0;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();

        intent.putExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), mGson.toJson(bloodPressureMeasurementCharacteristicData));

        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isMmhgReference.get());
        assertEquals(mFakeDeviceSettingRepository.getUnitString(isMmhgReference.get()), unitReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueSystolicMmhg.getSfloat(), Double.parseDouble(systolicReference.get()), 0);
        assertNull(systolicErrorStringReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueDiastolicMmhg.getSfloat(), Double.parseDouble(diastolicReference.get()), 0);
        assertNull(diastolicErrorStringReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg.getSfloat(), Double.parseDouble(meanArterialPressureReference.get()), 0);
        assertNull(meanArterialPressureErrorStringReference.get());
        assertFalse(isTimeStampSupportedReference.get());
        assertNull(timeStampYearReference.get());
        assertEquals(mFakeDeviceSettingRepository.getDateTimeYearErrorString(timeStampYearReference.get()), timeStampYearErrorStringReference.get());
        assertNull(timeStampMonthReference.get());
        assertNull(timeStampDayReference.get());
        assertNull(timeStampHoursReference.get());
        assertNull(timeStampMinutesReference.get());
        assertNull(timeStampSecondsReference.get());
        assertFalse(isPulseRateSupportedReference.get());
        assertNull(pulseRateReference.get());
        assertEquals(mFakeDeviceSettingRepository.getPulseRateErrorString(pulseRateReference.get()), pulseRateErrorStringReference.get());
        assertFalse(isUserIdSupportedReference.get());
        assertNull(userIdReference.get());
        assertEquals(mFakeDeviceSettingRepository.getUserIdErrorString(userIdReference.get()), userIdErrorStringReference.get());
        assertFalse(isMeasurementStatusSupportedReference.get());
        assertNull(bodyMovementDetectionReference.get());
        assertNull(cuffFitDetectionReference.get());
        assertNull(irregularPulseDetectionReference.get());
        assertNull(pulseRateRangeDetectionReference.get());
        assertNull(measurementPositionDetectionReference.get());
        assertNull(hasClientCharacteristicConfigurationDataJsonReference.get());
        assertEquals("", clientCharacteristicConfigurationReference.get());
        assertEquals(-1, Integer.parseInt(indicationCountReference.get()));
        assertNull(indicationCountErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();
        AtomicReference<String> unitReference = new AtomicReference<>();
        AtomicReference<String> systolicReference = new AtomicReference<>();
        AtomicReference<String> systolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> diastolicReference = new AtomicReference<>();
        AtomicReference<String> diastolicErrorStringReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();
        AtomicReference<String> meanArterialPressureErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearReference = new AtomicReference<>();
        AtomicReference<String> timeStampYearErrorStringReference = new AtomicReference<>();
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();
        AtomicReference<String> timeStampDayReference = new AtomicReference<>();
        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();
        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();
        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> pulseRateReference = new AtomicReference<>();
        AtomicReference<String> pulseRateErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> userIdReference = new AtomicReference<>();
        AtomicReference<String> userIdErrorStringReference = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();
        AtomicReference<String> irregularPulseDetectionReference = new AtomicReference<>();
        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();
        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();
        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();
        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();
        AtomicReference<String> indicationCountReference = new AtomicReference<>();
        AtomicReference<String> indicationCountErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), isMmhgReference::set);
        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);
        mViewModel.observeSystolic(new TestLifeCycleOwner(), systolicReference::set);
        mViewModel.observeSystolicErrorString(new TestLifeCycleOwner(), systolicErrorStringReference::set);
        mViewModel.observeDiastolic(new TestLifeCycleOwner(), diastolicReference::set);
        mViewModel.observeDiastolicErrorString(new TestLifeCycleOwner(), diastolicErrorStringReference::set);
        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), meanArterialPressureReference::set);
        mViewModel.observeMeanArterialPressureErrorString(new TestLifeCycleOwner(), meanArterialPressureErrorStringReference::set);
        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), isTimeStampSupportedReference::set);
        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), timeStampYearReference::set);
        mViewModel.observeTimeStampYearErrorString(new TestLifeCycleOwner(), timeStampYearErrorStringReference::set);
        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), timeStampMonthReference::set);
        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), timeStampDayReference::set);
        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), timeStampHoursReference::set);
        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), timeStampMinutesReference::set);
        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), timeStampSecondsReference::set);
        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), isPulseRateSupportedReference::set);
        mViewModel.observePulseRate(new TestLifeCycleOwner(), pulseRateReference::set);
        mViewModel.observePulseRateErrorString(new TestLifeCycleOwner(), pulseRateErrorStringReference::set);
        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), isUserIdSupportedReference::set);
        mViewModel.observeUserId(new TestLifeCycleOwner(), userIdReference::set);
        mViewModel.observeUserIdErrorString(new TestLifeCycleOwner(), userIdErrorStringReference::set);
        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), isMeasurementStatusSupportedReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetectionReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), irregularPulseDetectionReference::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetectionReference::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetectionReference::set);
        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), hasClientCharacteristicConfigurationDataJsonReference::set);
        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);
        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), indicationCountReference::set);
        mViewModel.observeIndicationCountErrorString(new TestLifeCycleOwner(), indicationCountErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData();
        bloodPressureMeasurementCharacteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
        bloodPressureMeasurementCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_BLOOD_PRESSURE_UNITS_KPA
                | BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT
                | BloodPressureMeasurementUtils.FLAG_PULSE_RATE_PRESENT
                | BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT
                | BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueSystolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueDiastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementCompoundValueMeanArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int bloodPressureMeasurementYear = 7777;
        int bloodPressureMeasurementMonth = 8;
        int bloodPressureMeasurementDay = 9;
        int bloodPressureMeasurementHours = 10;
        int bloodPressureMeasurementMinutes = 11;
        int bloodPressureMeasurementSeconds = 12;
        IEEE_11073_20601_SFLOAT bloodPressureMeasurementPulseRate = new IEEE_11073_20601_SFLOAT(13);
        int bloodPressureMeasurementUserId = 14;
        int bloodPressureMeasurementMeasurementStatusFlags = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
                , bloodPressureMeasurementCompoundValueSystolicMmhg
                , bloodPressureMeasurementCompoundValueDiastolicMmhg
                , bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg
                , bloodPressureMeasurementCompoundValueSystolicKpa
                , bloodPressureMeasurementCompoundValueDiastolicKpa
                , bloodPressureMeasurementCompoundValueMeanArterialPressureKpa
                , bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds
                , bloodPressureMeasurementPulseRate
                , bloodPressureMeasurementUserId
                , bloodPressureMeasurementMeasurementStatus);
        bloodPressureMeasurementCharacteristicData.data = bloodPressureMeasurement.getBytes();
        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE;
        bloodPressureMeasurementCharacteristicData.descriptorDataList.add(clientCharacteristicConfigurationDescriptorData);

        intent.putExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString(), mGson.toJson(bloodPressureMeasurementCharacteristicData));

        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isMmhgReference.get());
        assertEquals(mFakeDeviceSettingRepository.getUnitString(isMmhgReference.get()), unitReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueSystolicKpa.getSfloat(), Double.parseDouble(systolicReference.get()), 0);
        assertNull(systolicErrorStringReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueDiastolicKpa.getSfloat(), Double.parseDouble(diastolicReference.get()), 0);
        assertNull(diastolicErrorStringReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueMeanArterialPressureKpa.getSfloat(), Double.parseDouble(meanArterialPressureReference.get()), 0);
        assertNull(meanArterialPressureErrorStringReference.get());
        assertTrue(isTimeStampSupportedReference.get());
        assertEquals(bloodPressureMeasurementYear, Integer.parseInt(timeStampYearReference.get()));
        assertNull(timeStampYearErrorStringReference.get());
        {
            Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeMonthList().stream().filter(integerStringPair
                    -> integerStringPair.first == bloodPressureMeasurementMonth).findFirst();
            assertTrue(optional.isPresent());
            assertEquals(optional.get().second, timeStampMonthReference.get());
        }
        {
            Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeDayList().stream().filter(integerStringPair
                    -> integerStringPair.first == bloodPressureMeasurementDay).findFirst();
            assertTrue(optional.isPresent());
            assertEquals(optional.get().second, timeStampDayReference.get());
        }
        assertEquals(bloodPressureMeasurementHours, Integer.parseInt(timeStampHoursReference.get()));
        assertEquals(bloodPressureMeasurementMinutes, Integer.parseInt(timeStampMinutesReference.get()));
        assertEquals(bloodPressureMeasurementSeconds, Integer.parseInt(timeStampSecondsReference.get()));
        assertTrue(isPulseRateSupportedReference.get());
        assertEquals(bloodPressureMeasurementPulseRate.getSfloat(), Double.parseDouble(pulseRateReference.get()), 0);
        assertNull(pulseRateErrorStringReference.get());
        assertTrue(isUserIdSupportedReference.get());
        assertEquals(bloodPressureMeasurementUserId, Integer.parseInt(userIdReference.get()));
        assertNull(userIdErrorStringReference.get());
        assertTrue(isMeasurementStatusSupportedReference.get());
        {
            Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideBodyMovementDetectionList().stream().filter(integerStringPair
                    -> integerStringPair.first == (bloodPressureMeasurementMeasurementStatusFlags & MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK))
                    .findFirst();
            assertTrue(optional.isPresent());
            assertEquals(optional.get().second, bodyMovementDetectionReference.get());
        }
        {
            Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideCuffFitDetectionList().stream().filter(integerStringPair
                    -> integerStringPair.first == (bloodPressureMeasurementMeasurementStatusFlags & MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK))
                    .findFirst();
            assertTrue(optional.isPresent());
            assertEquals(optional.get().second, cuffFitDetectionReference.get());
        }
        {
            Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList().stream().filter(integerStringPair
                    -> integerStringPair.first == (bloodPressureMeasurementMeasurementStatusFlags & MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK))
                    .findFirst();
            assertTrue(optional.isPresent());
            assertEquals(optional.get().second, irregularPulseDetectionReference.get());
        }
        {
            Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList().stream().filter(integerStringPair
                    -> integerStringPair.first == (bloodPressureMeasurementMeasurementStatusFlags & MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK))
                    .findFirst();
            assertTrue(optional.isPresent());
            assertEquals(optional.get().second, pulseRateRangeDetectionReference.get());
        }
        {
            Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList().stream().filter(integerStringPair
                    -> integerStringPair.first == (bloodPressureMeasurementMeasurementStatusFlags & MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK))
                    .findFirst();
            assertTrue(optional.isPresent());
            assertEquals(optional.get().second, measurementPositionDetectionReference.get());
        }

        assertTrue(hasClientCharacteristicConfigurationDataJsonReference.get());
        assertEquals(mFakeDeviceSettingRepository.getIndicationsDisabledString(), clientCharacteristicConfigurationReference.get());
        assertEquals(-1, Integer.parseInt(indicationCountReference.get()));
        assertNull(indicationCountErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_4_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> observeSetupThrowable = new AtomicReference<>();

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.observeSetup(intent
                , () -> {
                }
                , observeSetupThrowable::set);

        TestCase.assertEquals("Initialized", observeSetupThrowable.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());
        TestCase.assertEquals("Already saved", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsTimeStampSupported(true);
        mViewModel.updateTimeStampMonth(0);
        mViewModel.updateTimeStampDay(0);
        mViewModel.updateTimeStampHours(0);
        mViewModel.updateTimeStampMinutes(0);
        mViewModel.updateTimeStampSeconds(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00006() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsTimeStampSupported(true);
        mViewModel.updateTimeStampYear("5555");
        mViewModel.updateTimeStampDay(0);
        mViewModel.updateTimeStampHours(0);
        mViewModel.updateTimeStampMinutes(0);
        mViewModel.updateTimeStampSeconds(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00007() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsTimeStampSupported(true);
        mViewModel.updateTimeStampYear("5555");
        mViewModel.updateTimeStampMonth(0);
        mViewModel.updateTimeStampHours(0);
        mViewModel.updateTimeStampMinutes(0);
        mViewModel.updateTimeStampSeconds(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00008() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsTimeStampSupported(true);
        mViewModel.updateTimeStampYear("5555");
        mViewModel.updateTimeStampMonth(0);
        mViewModel.updateTimeStampDay(0);
        mViewModel.updateTimeStampMinutes(0);
        mViewModel.updateTimeStampSeconds(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00009() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsTimeStampSupported(true);
        mViewModel.updateTimeStampYear("5555");
        mViewModel.updateTimeStampMonth(0);
        mViewModel.updateTimeStampDay(0);
        mViewModel.updateTimeStampHours(0);
        mViewModel.updateTimeStampSeconds(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00010() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsTimeStampSupported(true);
        mViewModel.updateTimeStampYear("5555");
        mViewModel.updateTimeStampMonth(0);
        mViewModel.updateTimeStampDay(0);
        mViewModel.updateTimeStampHours(0);
        mViewModel.updateTimeStampMinutes(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00011() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsPulseRateSupported(true);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00012() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsUserIdSupported(true);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00013() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsMeasurementStatusSupported(true);
        mViewModel.updateCuffFitDetection(0);
        mViewModel.updateIrregularPulseDetection(0);
        mViewModel.updatePulseRateRangeDetection(0);
        mViewModel.updateMeasurementPositionDetection(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00014() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsMeasurementStatusSupported(true);
        mViewModel.updateBodyMovementDetection(0);
        mViewModel.updateIrregularPulseDetection(0);
        mViewModel.updatePulseRateRangeDetection(0);
        mViewModel.updateMeasurementPositionDetection(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00015() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsMeasurementStatusSupported(true);
        mViewModel.updateBodyMovementDetection(0);
        mViewModel.updateCuffFitDetection(0);
        mViewModel.updatePulseRateRangeDetection(0);
        mViewModel.updateMeasurementPositionDetection(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00016() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsMeasurementStatusSupported(true);
        mViewModel.updateBodyMovementDetection(0);
        mViewModel.updateCuffFitDetection(0);
        mViewModel.updateIrregularPulseDetection(0);
        mViewModel.updateMeasurementPositionDetection(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00017() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIsMeasurementStatusSupported(true);
        mViewModel.updateBodyMovementDetection(0);
        mViewModel.updateCuffFitDetection(0);
        mViewModel.updateIrregularPulseDetection(0);
        mViewModel.updatePulseRateRangeDetection(0);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00018() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        mViewModel.updateIndicationCount("");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_1_00019() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateSystolic("1");
        mViewModel.updateDiastolic("2");
        mViewModel.updateMeanArterialPressure("3");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        TestCase.assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        double systolic = 1;
        double diastolic = 2;
        double meanArterialPressure = 3;
        mViewModel.updateSystolic(String.valueOf(systolic));
        mViewModel.updateDiastolic(String.valueOf(diastolic));
        mViewModel.updateMeanArterialPressure(String.valueOf(meanArterialPressure));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
        TestCase.assertEquals(systolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat());
        TestCase.assertEquals(diastolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat());
        TestCase.assertEquals(meanArterialPressure, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat());

        Optional<DescriptorData> descriptorDataOptional = characteristicData.descriptorDataList.stream().filter(descriptorData
                -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR)).findFirst();
        assertTrue(descriptorDataOptional.isPresent());
        assertArrayEquals(clientCharacteristicConfigurationDescriptorData.data, descriptorDataOptional.get().data);
    }

    @Test
    public void test_observeSave_2_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        double systolic = 1;
        double diastolic = 2;
        double meanArterialPressure = 3;
        mViewModel.updateSystolic(String.valueOf(systolic));
        mViewModel.updateDiastolic(String.valueOf(diastolic));
        mViewModel.updateMeanArterialPressure(String.valueOf(meanArterialPressure));
        mViewModel.updateIsMmhg(false);

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
        TestCase.assertEquals(systolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicKpa().getSfloat());
        TestCase.assertEquals(diastolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicKpa().getSfloat());
        TestCase.assertEquals(meanArterialPressure, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureKpa().getSfloat());

        Optional<DescriptorData> descriptorDataOptional = characteristicData.descriptorDataList.stream().filter(descriptorData
                -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR)).findFirst();
        assertTrue(descriptorDataOptional.isPresent());
        assertArrayEquals(clientCharacteristicConfigurationDescriptorData.data, descriptorDataOptional.get().data);
    }

    @Test
    public void test_observeSave_2_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        double systolic = 1;
        double diastolic = 2;
        double meanArterialPressure = 3;
        int year = 6666;
        int month = 1;
        int day = 2;
        int hours = 3;
        int minutes = 4;
        int seconds = 5;
        mViewModel.updateSystolic(String.valueOf(systolic));
        mViewModel.updateDiastolic(String.valueOf(diastolic));
        mViewModel.updateMeanArterialPressure(String.valueOf(meanArterialPressure));

        mViewModel.updateIsTimeStampSupported(true);
        mViewModel.updateTimeStampYear(String.valueOf(year));
        mViewModel.updateTimeStampMonth(month);
        mViewModel.updateTimeStampDay(day);
        mViewModel.updateTimeStampHours(hours);
        mViewModel.updateTimeStampMinutes(minutes);
        mViewModel.updateTimeStampSeconds(seconds);

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
        TestCase.assertEquals(systolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat());
        TestCase.assertEquals(diastolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat());
        TestCase.assertEquals(meanArterialPressure, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat());
        assertEquals(year, bloodPressureMeasurement.getYear());
        assertEquals(month, bloodPressureMeasurement.getMonth());
        assertEquals(day, bloodPressureMeasurement.getDay());
        assertEquals(hours, bloodPressureMeasurement.getHours());
        assertEquals(minutes, bloodPressureMeasurement.getMinutes());
        assertEquals(seconds, bloodPressureMeasurement.getSeconds());

        Optional<DescriptorData> descriptorDataOptional = characteristicData.descriptorDataList.stream().filter(descriptorData
                -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR)).findFirst();
        assertTrue(descriptorDataOptional.isPresent());
        assertArrayEquals(clientCharacteristicConfigurationDescriptorData.data, descriptorDataOptional.get().data);
    }

    @Test
    public void test_observeSave_2_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        double systolic = 1;
        double diastolic = 2;
        double meanArterialPressure = 3;
        double pulseRate = 4;
        mViewModel.updateSystolic(String.valueOf(systolic));
        mViewModel.updateDiastolic(String.valueOf(diastolic));
        mViewModel.updateMeanArterialPressure(String.valueOf(meanArterialPressure));

        mViewModel.updateIsPulseRateSupported(true);
        mViewModel.updatePulseRate(String.valueOf(pulseRate));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
        TestCase.assertEquals(systolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat());
        TestCase.assertEquals(diastolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat());
        TestCase.assertEquals(meanArterialPressure, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat());
        TestCase.assertEquals(pulseRate, bloodPressureMeasurement.getPulseRate().getSfloat());

        Optional<DescriptorData> descriptorDataOptional = characteristicData.descriptorDataList.stream().filter(descriptorData
                -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR)).findFirst();
        assertTrue(descriptorDataOptional.isPresent());
        assertArrayEquals(clientCharacteristicConfigurationDescriptorData.data, descriptorDataOptional.get().data);
    }

    @Test
    public void test_observeSave_2_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        double systolic = 1;
        double diastolic = 2;
        double meanArterialPressure = 3;
        int userId = 4;
        mViewModel.updateSystolic(String.valueOf(systolic));
        mViewModel.updateDiastolic(String.valueOf(diastolic));
        mViewModel.updateMeanArterialPressure(String.valueOf(meanArterialPressure));

        mViewModel.updateIsUserIdSupported(true);
        mViewModel.updateUserId(String.valueOf(userId));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
        TestCase.assertEquals(systolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat());
        TestCase.assertEquals(diastolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat());
        TestCase.assertEquals(meanArterialPressure, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat());
        TestCase.assertEquals(userId, bloodPressureMeasurement.getUserId());

        Optional<DescriptorData> descriptorDataOptional = characteristicData.descriptorDataList.stream().filter(descriptorData
                -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR)).findFirst();
        assertTrue(descriptorDataOptional.isPresent());
        assertArrayEquals(clientCharacteristicConfigurationDescriptorData.data, descriptorDataOptional.get().data);
    }

    @Test
    public void test_observeSave_2_00006() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        double systolic = 1;
        double diastolic = 2;
        double meanArterialPressure = 3;
        int bodyMovementDetectionIndex = mFakeDeviceSettingRepository.provideBodyMovementDetectionList().size() - 1;
        Pair<Integer, String> bodyMovementDetection = mFakeDeviceSettingRepository.provideBodyMovementDetectionList().get(bodyMovementDetectionIndex);
        int cuffFitDetectionIndex = mFakeDeviceSettingRepository.provideCuffFitDetectionList().size() - 1;
        Pair<Integer, String> cuffFitDetection = mFakeDeviceSettingRepository.provideCuffFitDetectionList().get(cuffFitDetectionIndex);
        int irregularPulseDetectionIndex = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList().size() - 1;
        Pair<Integer, String> irregularPulseDetection = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList().get(irregularPulseDetectionIndex);
        int pulseRateRangeDetectionIndex = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList().size() - 1;
        Pair<Integer, String> pulseRateRangeDetection = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList().get(pulseRateRangeDetectionIndex);
        int measurementPositionDetectionIndex = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList().size() - 1;
        Pair<Integer, String> measurementPositionDetection = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList().get(measurementPositionDetectionIndex);

        mViewModel.updateSystolic(String.valueOf(systolic));
        mViewModel.updateDiastolic(String.valueOf(diastolic));
        mViewModel.updateMeanArterialPressure(String.valueOf(meanArterialPressure));

        mViewModel.updateIsMeasurementStatusSupported(true);
        mViewModel.updateBodyMovementDetection(bodyMovementDetectionIndex);
        mViewModel.updateCuffFitDetection(cuffFitDetectionIndex);
        mViewModel.updateIrregularPulseDetection(irregularPulseDetectionIndex);
        mViewModel.updatePulseRateRangeDetection(pulseRateRangeDetectionIndex);
        mViewModel.updateMeasurementPositionDetection(measurementPositionDetectionIndex);

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
        TestCase.assertEquals(systolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat());
        TestCase.assertEquals(diastolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat());
        TestCase.assertEquals(meanArterialPressure, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat());

        int measurementStatus = BLEUtils.createUInt16(bloodPressureMeasurement.getMeasurementStatus(), 0);
        TestCase.assertEquals(bodyMovementDetection.first.intValue(), measurementStatus & MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK);
        TestCase.assertEquals(cuffFitDetection.first.intValue(), measurementStatus & MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK);
        TestCase.assertEquals(irregularPulseDetection.first.intValue(), measurementStatus & MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK);
        TestCase.assertEquals(pulseRateRangeDetection.first.intValue(), measurementStatus & MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK);
        TestCase.assertEquals(measurementPositionDetection.first.intValue(), measurementStatus & MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK);

        Optional<DescriptorData> descriptorDataOptional = characteristicData.descriptorDataList.stream().filter(descriptorData
                -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR)).findFirst();
        assertTrue(descriptorDataOptional.isPresent());
        assertArrayEquals(clientCharacteristicConfigurationDescriptorData.data, descriptorDataOptional.get().data);
    }

    @Test
    public void test_observeSave_2_00007() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        double systolic = 1;
        double diastolic = 2;
        double meanArterialPressure = 3;

        int indicationCount = 4;

        mViewModel.updateSystolic(String.valueOf(systolic));
        mViewModel.updateDiastolic(String.valueOf(diastolic));
        mViewModel.updateMeanArterialPressure(String.valueOf(meanArterialPressure));

        mViewModel.updateIndicationCount(String.valueOf(indicationCount));

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(mGson.toJson(clientCharacteristicConfigurationDescriptorData));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> characteristicDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC.toString()), CharacteristicData.class))
                , throwable -> {
                });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(characteristicData.data);
        TestCase.assertEquals(systolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueSystolicMmhg().getSfloat());
        TestCase.assertEquals(diastolic, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueDiastolicMmhg().getSfloat());
        TestCase.assertEquals(meanArterialPressure, bloodPressureMeasurement.getBloodPressureMeasurementCompoundValueMeanArterialPressureMmhg().getSfloat());

        TestCase.assertEquals(indicationCount, characteristicData.notificationCount);

        Optional<DescriptorData> descriptorDataOptional = characteristicData.descriptorDataList.stream().filter(descriptorData
                -> descriptorData.uuid.equals(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR)).findFirst();
        assertTrue(descriptorDataOptional.isPresent());
        assertArrayEquals(clientCharacteristicConfigurationDescriptorData.data, descriptorDataOptional.get().data);
    }

    @Test
    public void test_observeIsMmhg_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();

        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), isMmhgReference::set);

        TestCase.assertNull(isMmhgReference.get());
    }

    @Test
    public void test_observeIsMmhg_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_MMHG", original);
        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), isMmhgReference::set);

        TestCase.assertEquals(original, isMmhgReference.get().booleanValue());
    }

    @Test
    public void test_observeIsMmhg_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();

        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), isMmhgReference::set);
        mSavedStateHandle.set("KEY_IS_MMHG", original);

        TestCase.assertEquals(original, isMmhgReference.get().booleanValue());
    }

    @Test
    public void test_observeIsMmhg_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isMmhgReference = new AtomicReference<>();

        mViewModel.observeIsMmhg(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isMmhgReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_MMHG", original);
        mSavedStateHandle.set("KEY_IS_MMHG", original);

        TestCase.assertEquals(original, isMmhgReference.get().booleanValue());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeUnit_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> unitReference = new AtomicReference<>();

        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);

        TestCase.assertNull(unitReference.get());
    }

    @Test
    public void test_observeUnit_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<String> unitReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_MMHG", original);
        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);

        TestCase.assertEquals(mFakeDeviceSettingRepository.getUnitString(original), unitReference.get());
    }

    @Test
    public void test_observeUnit_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<String> unitReference = new AtomicReference<>();

        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);
        mSavedStateHandle.set("KEY_IS_MMHG", original);

        TestCase.assertEquals(mFakeDeviceSettingRepository.getUnitString(original), unitReference.get());
    }

    @Test
    public void test_observeUnit_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> unitReference = new AtomicReference<>();

        mViewModel.observeUnit(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            unitReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_MMHG", original);
        mSavedStateHandle.set("KEY_IS_MMHG", original);

        TestCase.assertEquals(mFakeDeviceSettingRepository.getUnitString(original), unitReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeUnit_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = false;
        AtomicReference<String> unitReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_MMHG", original);
        mViewModel.observeUnit(new TestLifeCycleOwner(), unitReference::set);

        TestCase.assertEquals(mFakeDeviceSettingRepository.getUnitString(original), unitReference.get());
    }

    @Test
    public void test_observeSystolic_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> systolicReference = new AtomicReference<>();

        mViewModel.observeSystolic(new TestLifeCycleOwner(), systolicReference::set);

        TestCase.assertNull(systolicReference.get());
    }

    @Test
    public void test_observeSystolic_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> systolicReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_SYSTOLIC", original);
        mViewModel.observeSystolic(new TestLifeCycleOwner(), systolicReference::set);

        TestCase.assertEquals(original, systolicReference.get());
    }

    @Test
    public void test_observeSystolic_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> systolicReference = new AtomicReference<>();

        mViewModel.observeSystolic(new TestLifeCycleOwner(), systolicReference::set);
        mSavedStateHandle.set("KEY_SYSTOLIC", original);

        TestCase.assertEquals(original, systolicReference.get());
    }

    @Test
    public void test_observeSystolic_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> systolicReference = new AtomicReference<>();

        mViewModel.observeSystolic(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            systolicReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_SYSTOLIC", original);
        mSavedStateHandle.set("KEY_SYSTOLIC", original);

        TestCase.assertEquals(original, systolicReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeSystolicErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetSystolicErrorString = "a";
        AtomicReference<String> systolicErrorStringReference = new AtomicReference<>();

        mViewModel.observeSystolicErrorString(new TestLifeCycleOwner(), systolicErrorStringReference::set);

        TestCase.assertNull(systolicErrorStringReference.get());
    }

    @Test
    public void test_observeSystolicErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetSystolicErrorString = original;
        AtomicReference<String> systolicErrorStringReference = new AtomicReference<>();

        mViewModel.observeSystolicErrorString(new TestLifeCycleOwner(), systolicErrorStringReference::set);
        mViewModel.updateSystolic(null);

        TestCase.assertEquals(original, systolicErrorStringReference.get());
    }

    @Test
    public void test_observeSystolicErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetSystolicErrorString = original;
        AtomicReference<String> systolicErrorStringReference = new AtomicReference<>();

        mViewModel.observeSystolicErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            systolicErrorStringReference.set(s);
        });
        mViewModel.updateSystolic(null);
        mViewModel.updateSystolic(null);

        TestCase.assertEquals(original, systolicErrorStringReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeDiastolic_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> diastolicReference = new AtomicReference<>();

        mViewModel.observeDiastolic(new TestLifeCycleOwner(), diastolicReference::set);

        TestCase.assertNull(diastolicReference.get());
    }

    @Test
    public void test_observeDiastolic_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> diastolicReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DIASTOLIC", original);
        mViewModel.observeDiastolic(new TestLifeCycleOwner(), diastolicReference::set);

        TestCase.assertEquals(original, diastolicReference.get());
    }

    @Test
    public void test_observeDiastolic_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> diastolicReference = new AtomicReference<>();

        mViewModel.observeDiastolic(new TestLifeCycleOwner(), diastolicReference::set);
        mSavedStateHandle.set("KEY_DIASTOLIC", original);

        TestCase.assertEquals(original, diastolicReference.get());
    }

    @Test
    public void test_observeDiastolic_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> diastolicReference = new AtomicReference<>();

        mViewModel.observeDiastolic(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            diastolicReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_DIASTOLIC", original);
        mSavedStateHandle.set("KEY_DIASTOLIC", original);

        TestCase.assertEquals(original, diastolicReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeDiastolicErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetDiastolicErrorString = "a";
        AtomicReference<String> diastolicErrorStringReference = new AtomicReference<>();

        mViewModel.observeDiastolicErrorString(new TestLifeCycleOwner(), diastolicErrorStringReference::set);

        TestCase.assertNull(diastolicErrorStringReference.get());
    }

    @Test
    public void test_observeDiastolicErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetDiastolicErrorString = original;
        AtomicReference<String> diastolicErrorStringReference = new AtomicReference<>();

        mViewModel.observeDiastolicErrorString(new TestLifeCycleOwner(), diastolicErrorStringReference::set);
        mViewModel.updateDiastolic(null);

        TestCase.assertEquals(original, diastolicErrorStringReference.get());
    }

    @Test
    public void test_observeDiastolicErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetDiastolicErrorString = original;
        AtomicReference<String> diastolicErrorStringReference = new AtomicReference<>();

        mViewModel.observeDiastolicErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            diastolicErrorStringReference.set(s);
        });
        mViewModel.updateDiastolic(null);
        mViewModel.updateDiastolic(null);

        TestCase.assertEquals(original, diastolicErrorStringReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeMeanArterialPressure_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();

        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), meanArterialPressureReference::set);

        TestCase.assertNull(meanArterialPressureReference.get());
    }

    @Test
    public void test_observeMeanArterialPressure_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MEAN_ARTERIAL_PRESSURE", original);
        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), meanArterialPressureReference::set);

        TestCase.assertEquals(original, meanArterialPressureReference.get());
    }

    @Test
    public void test_observeMeanArterialPressure_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();

        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), meanArterialPressureReference::set);
        mSavedStateHandle.set("KEY_MEAN_ARTERIAL_PRESSURE", original);

        TestCase.assertEquals(original, meanArterialPressureReference.get());
    }

    @Test
    public void test_observeMeanArterialPressure_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> meanArterialPressureReference = new AtomicReference<>();

        mViewModel.observeMeanArterialPressure(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            meanArterialPressureReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MEAN_ARTERIAL_PRESSURE", original);
        mSavedStateHandle.set("KEY_MEAN_ARTERIAL_PRESSURE", original);

        TestCase.assertEquals(original, meanArterialPressureReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeMeanArterialPressureErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetMeanArterialPressureErrorString = "a";
        AtomicReference<String> meanArterialPressureErrorStringReference = new AtomicReference<>();

        mViewModel.observeMeanArterialPressureErrorString(new TestLifeCycleOwner(), meanArterialPressureErrorStringReference::set);

        TestCase.assertNull(meanArterialPressureErrorStringReference.get());
    }

    @Test
    public void test_observeMeanArterialPressureErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetMeanArterialPressureErrorString = original;
        AtomicReference<String> meanArterialPressureErrorStringReference = new AtomicReference<>();

        mViewModel.observeMeanArterialPressureErrorString(new TestLifeCycleOwner(), meanArterialPressureErrorStringReference::set);
        mViewModel.updateMeanArterialPressure(null);

        TestCase.assertEquals(original, meanArterialPressureErrorStringReference.get());
    }

    @Test
    public void test_observeMeanArterialPressureErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetMeanArterialPressureErrorString = original;
        AtomicReference<String> meanArterialPressureErrorStringReference = new AtomicReference<>();

        mViewModel.observeMeanArterialPressureErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            meanArterialPressureErrorStringReference.set(s);
        });
        mViewModel.updateMeanArterialPressure(null);
        mViewModel.updateMeanArterialPressure(null);

        TestCase.assertEquals(original, meanArterialPressureErrorStringReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsTimeStampSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();

        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), isTimeStampSupportedReference::set);

        TestCase.assertNull(isTimeStampSupportedReference.get());
    }

    @Test
    public void test_observeIsTimeStampSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", original);
        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), isTimeStampSupportedReference::set);

        TestCase.assertEquals(original, isTimeStampSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsTimeStampSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();

        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), isTimeStampSupportedReference::set);
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", original);

        TestCase.assertEquals(original, isTimeStampSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsTimeStampSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isTimeStampSupportedReference = new AtomicReference<>();

        mViewModel.observeIsTimeStampSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isTimeStampSupportedReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", original);
        mSavedStateHandle.set("KEY_IS_TIME_STAMP_SUPPORTED", original);

        TestCase.assertEquals(original, isTimeStampSupportedReference.get().booleanValue());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeTimeStampYear_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> timeStampYearReference = new AtomicReference<>();

        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), timeStampYearReference::set);

        TestCase.assertNull(timeStampYearReference.get());
    }

    @Test
    public void test_observeTimeStampYear_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> timeStampYearReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TIME_STAMP_YEAR", original);
        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), timeStampYearReference::set);

        TestCase.assertEquals(original, timeStampYearReference.get());
    }

    @Test
    public void test_observeTimeStampYear_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> timeStampYearReference = new AtomicReference<>();

        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), timeStampYearReference::set);
        mSavedStateHandle.set("KEY_TIME_STAMP_YEAR", original);

        TestCase.assertEquals(original, timeStampYearReference.get());
    }

    @Test
    public void test_observeTimeStampYear_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> timeStampYearReference = new AtomicReference<>();

        mViewModel.observeTimeStampYear(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            timeStampYearReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_TIME_STAMP_YEAR", original);
        mSavedStateHandle.set("KEY_TIME_STAMP_YEAR", original);

        TestCase.assertEquals(original, timeStampYearReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeTimeStampYearErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetDateTimeYearErrorString = "a";
        AtomicReference<String> timeStampYearErrorStringReference = new AtomicReference<>();

        mViewModel.observeTimeStampYearErrorString(new TestLifeCycleOwner(), timeStampYearErrorStringReference::set);

        TestCase.assertNull(timeStampYearErrorStringReference.get());
    }

    @Test
    public void test_observeTimeStampYearErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetDateTimeYearErrorString = original;
        AtomicReference<String> timeStampYearErrorStringReference = new AtomicReference<>();

        mViewModel.observeTimeStampYearErrorString(new TestLifeCycleOwner(), timeStampYearErrorStringReference::set);
        mViewModel.updateTimeStampYear(null);

        TestCase.assertEquals(original, timeStampYearErrorStringReference.get());
    }

    @Test
    public void test_observeTimeStampYearErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetDateTimeYearErrorString = original;
        AtomicReference<String> timeStampYearErrorStringReference = new AtomicReference<>();

        mViewModel.observeTimeStampYearErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            timeStampYearErrorStringReference.set(s);
        });
        mViewModel.updateTimeStampYear(null);
        mViewModel.updateTimeStampYear(null);

        TestCase.assertEquals(original, timeStampYearErrorStringReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeTimeStampMonth_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), timeStampMonthReference::set);

        TestCase.assertNull(timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampMonth_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TIME_STAMP_MONTH", original);
        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), timeStampMonthReference::set);

        Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeMonthList().stream().filter(integerStringPair
                -> integerStringPair.first == original).findFirst();
        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampMonth_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), timeStampMonthReference::set);
        mSavedStateHandle.set("KEY_TIME_STAMP_MONTH", original);

        Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeMonthList().stream().filter(integerStringPair
                -> integerStringPair.first == original).findFirst();
        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampMonth_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mViewModel.observeTimeStampMonth(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            timeStampMonthReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_TIME_STAMP_MONTH", original);
        mSavedStateHandle.set("KEY_TIME_STAMP_MONTH", original);

        Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeMonthList().stream().filter(integerStringPair
                -> integerStringPair.first == original).findFirst();
        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampDay_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), timeStampMonthReference::set);

        TestCase.assertNull(timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampDay_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TIME_STAMP_DAY", original);
        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), timeStampMonthReference::set);

        Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeDayList().stream().filter(integerStringPair
                -> integerStringPair.first == original).findFirst();
        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampDay_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), timeStampMonthReference::set);
        mSavedStateHandle.set("KEY_TIME_STAMP_DAY", original);

        Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeDayList().stream().filter(integerStringPair
                -> integerStringPair.first == original).findFirst();
        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampDay_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> timeStampMonthReference = new AtomicReference<>();

        mViewModel.observeTimeStampDay(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            timeStampMonthReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_TIME_STAMP_DAY", original);
        mSavedStateHandle.set("KEY_TIME_STAMP_DAY", original);

        Optional<Pair<Integer, String>> optional = mFakeDeviceSettingRepository.provideDateTimeDayList().stream().filter(integerStringPair
                -> integerStringPair.first == original).findFirst();
        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, timeStampMonthReference.get());
    }

    @Test
    public void test_observeTimeStampHours_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();

        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), timeStampHoursReference::set);

        TestCase.assertNull(timeStampHoursReference.get());
    }

    @Test
    public void test_observeTimeStampHours_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TIME_STAMP_HOURS", original);
        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), timeStampHoursReference::set);

        assertEquals(original, Integer.parseInt(timeStampHoursReference.get()));
    }

    @Test
    public void test_observeTimeStampHours_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();

        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), timeStampHoursReference::set);
        mSavedStateHandle.set("KEY_TIME_STAMP_HOURS", original);

        assertEquals(original, Integer.parseInt(timeStampHoursReference.get()));
    }

    @Test
    public void test_observeTimeStampHours_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> timeStampHoursReference = new AtomicReference<>();

        mViewModel.observeTimeStampHours(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            timeStampHoursReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_TIME_STAMP_HOURS", original);
        mSavedStateHandle.set("KEY_TIME_STAMP_HOURS", original);

        assertEquals(original, Integer.parseInt(timeStampHoursReference.get()));
    }

    @Test
    public void test_observeTimeStampMinutes_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();

        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), timeStampMinutesReference::set);

        TestCase.assertNull(timeStampMinutesReference.get());
    }

    @Test
    public void test_observeTimeStampMinutes_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TIME_STAMP_MINUTES", original);
        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), timeStampMinutesReference::set);

        assertEquals(original, Integer.parseInt(timeStampMinutesReference.get()));
    }

    @Test
    public void test_observeTimeStampMinutes_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();

        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), timeStampMinutesReference::set);
        mSavedStateHandle.set("KEY_TIME_STAMP_MINUTES", original);

        assertEquals(original, Integer.parseInt(timeStampMinutesReference.get()));
    }

    @Test
    public void test_observeTimeStampMinutes_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> timeStampMinutesReference = new AtomicReference<>();

        mViewModel.observeTimeStampMinutes(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            timeStampMinutesReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_TIME_STAMP_MINUTES", original);
        mSavedStateHandle.set("KEY_TIME_STAMP_MINUTES", original);

        assertEquals(original, Integer.parseInt(timeStampMinutesReference.get()));
    }

    @Test
    public void test_observeTimeStampSeconds_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();

        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), timeStampSecondsReference::set);

        TestCase.assertNull(timeStampSecondsReference.get());
    }

    @Test
    public void test_observeTimeStampSeconds_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TIME_STAMP_SECONDS", original);
        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), timeStampSecondsReference::set);

        assertEquals(original, Integer.parseInt(timeStampSecondsReference.get()));
    }

    @Test
    public void test_observeTimeStampSeconds_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();

        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), timeStampSecondsReference::set);
        mSavedStateHandle.set("KEY_TIME_STAMP_SECONDS", original);

        assertEquals(original, Integer.parseInt(timeStampSecondsReference.get()));
    }

    @Test
    public void test_observeTimeStampSeconds_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> timeStampSecondsReference = new AtomicReference<>();

        mViewModel.observeTimeStampSeconds(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            timeStampSecondsReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_TIME_STAMP_SECONDS", original);
        mSavedStateHandle.set("KEY_TIME_STAMP_SECONDS", original);

        assertEquals(original, Integer.parseInt(timeStampSecondsReference.get()));
    }

    @Test
    public void test_observeIsPulseRateSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();

        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), isPulseRateSupportedReference::set);

        TestCase.assertNull(isPulseRateSupportedReference.get());
    }

    @Test
    public void test_observeIsPulseRateSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", original);
        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), isPulseRateSupportedReference::set);

        TestCase.assertEquals(original, isPulseRateSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsPulseRateSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();

        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), isPulseRateSupportedReference::set);
        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", original);

        TestCase.assertEquals(original, isPulseRateSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsPulseRateSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isPulseRateSupportedReference = new AtomicReference<>();

        mViewModel.observeIsPulseRateSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isPulseRateSupportedReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", original);
        mSavedStateHandle.set("KEY_IS_PULSE_RATE_SUPPORTED", original);

        TestCase.assertEquals(original, isPulseRateSupportedReference.get().booleanValue());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observePulseRate_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> pulseRateReference = new AtomicReference<>();

        mViewModel.observePulseRate(new TestLifeCycleOwner(), pulseRateReference::set);

        TestCase.assertNull(pulseRateReference.get());
    }

    @Test
    public void test_observePulseRate_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> pulseRateReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_PULSE_RATE", original);
        mViewModel.observePulseRate(new TestLifeCycleOwner(), pulseRateReference::set);

        TestCase.assertEquals(original, pulseRateReference.get());
    }

    @Test
    public void test_observePulseRate_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> pulseRateReference = new AtomicReference<>();

        mViewModel.observePulseRate(new TestLifeCycleOwner(), pulseRateReference::set);
        mSavedStateHandle.set("KEY_PULSE_RATE", original);

        TestCase.assertEquals(original, pulseRateReference.get());
    }

    @Test
    public void test_observePulseRate_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> pulseRateReference = new AtomicReference<>();

        mViewModel.observePulseRate(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            pulseRateReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_PULSE_RATE", original);
        mSavedStateHandle.set("KEY_PULSE_RATE", original);

        TestCase.assertEquals(original, pulseRateReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observePulseRateErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetPulseRateErrorString = "a";
        AtomicReference<String> pulseRateErrorStringReference = new AtomicReference<>();

        mViewModel.observePulseRateErrorString(new TestLifeCycleOwner(), pulseRateErrorStringReference::set);

        TestCase.assertNull(pulseRateErrorStringReference.get());
    }

    @Test
    public void test_observePulseRateErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetPulseRateErrorString = original;
        AtomicReference<String> pulseRateErrorStringReference = new AtomicReference<>();

        mViewModel.observePulseRateErrorString(new TestLifeCycleOwner(), pulseRateErrorStringReference::set);
        mViewModel.updatePulseRate(null);

        TestCase.assertEquals(original, pulseRateErrorStringReference.get());
    }

    @Test
    public void test_observePulseRateErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetPulseRateErrorString = original;
        AtomicReference<String> pulseRateErrorStringReference = new AtomicReference<>();

        mViewModel.observePulseRateErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            pulseRateErrorStringReference.set(s);
        });
        mViewModel.updatePulseRate(null);
        mViewModel.updatePulseRate(null);

        TestCase.assertEquals(original, pulseRateErrorStringReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsUserIdSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();

        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), isUserIdSupportedReference::set);

        TestCase.assertNull(isUserIdSupportedReference.get());
    }

    @Test
    public void test_observeIsUserIdSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", original);
        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), isUserIdSupportedReference::set);

        TestCase.assertEquals(original, isUserIdSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsUserIdSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();

        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), isUserIdSupportedReference::set);
        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", original);

        TestCase.assertEquals(original, isUserIdSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsUserIdSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isUserIdSupportedReference = new AtomicReference<>();

        mViewModel.observeIsUserIdSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isUserIdSupportedReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", original);
        mSavedStateHandle.set("KEY_IS_USER_ID_SUPPORTED", original);

        TestCase.assertEquals(original, isUserIdSupportedReference.get().booleanValue());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeUserId_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> pulseRateReference = new AtomicReference<>();

        mViewModel.observeUserId(new TestLifeCycleOwner(), pulseRateReference::set);

        TestCase.assertNull(pulseRateReference.get());
    }

    @Test
    public void test_observeUserId_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> userIdReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_USER_ID", original);
        mViewModel.observeUserId(new TestLifeCycleOwner(), userIdReference::set);

        TestCase.assertEquals(original, userIdReference.get());
    }

    @Test
    public void test_observeUserId_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> userIdReference = new AtomicReference<>();

        mViewModel.observeUserId(new TestLifeCycleOwner(), userIdReference::set);
        mSavedStateHandle.set("KEY_USER_ID", original);

        TestCase.assertEquals(original, userIdReference.get());
    }

    @Test
    public void test_observeUserId_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> userIdReference = new AtomicReference<>();

        mViewModel.observeUserId(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            userIdReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_USER_ID", original);
        mSavedStateHandle.set("KEY_USER_ID", original);

        TestCase.assertEquals(original, userIdReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeUserIdErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetUserIdErrorString = "a";
        AtomicReference<String> userIdErrorStringReference = new AtomicReference<>();

        mViewModel.observeUserIdErrorString(new TestLifeCycleOwner(), userIdErrorStringReference::set);

        TestCase.assertNull(userIdErrorStringReference.get());
    }

    @Test
    public void test_observeUserIdErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetUserIdErrorString = original;
        AtomicReference<String> userIdErrorStringReference = new AtomicReference<>();

        mViewModel.observeUserIdErrorString(new TestLifeCycleOwner(), userIdErrorStringReference::set);
        mViewModel.updateUserId(null);

        TestCase.assertEquals(original, userIdErrorStringReference.get());
    }

    @Test
    public void test_observeUserIdErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetUserIdErrorString = original;
        AtomicReference<String> userIdErrorStringReference = new AtomicReference<>();

        mViewModel.observeUserIdErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            userIdErrorStringReference.set(s);
        });
        mViewModel.updateUserId(null);
        mViewModel.updateUserId(null);

        TestCase.assertEquals(original, userIdErrorStringReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsMeasurementStatusSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();

        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), isMeasurementStatusSupportedReference::set);

        TestCase.assertNull(isMeasurementStatusSupportedReference.get());
    }

    @Test
    public void test_observeIsMeasurementStatusSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", original);
        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), isMeasurementStatusSupportedReference::set);

        TestCase.assertEquals(original, isMeasurementStatusSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsMeasurementStatusSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();

        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), isMeasurementStatusSupportedReference::set);
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", original);

        TestCase.assertEquals(original, isMeasurementStatusSupportedReference.get().booleanValue());
    }

    @Test
    public void test_observeIsMeasurementStatusSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isMeasurementStatusSupportedReference = new AtomicReference<>();

        mViewModel.observeIsMeasurementStatusSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isMeasurementStatusSupportedReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", original);
        mSavedStateHandle.set("KEY_IS_MEASUREMENT_STATUS_SUPPORTED", original);

        TestCase.assertEquals(original, isMeasurementStatusSupportedReference.get().booleanValue());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeBodyMovementDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();

        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetectionReference::set);

        TestCase.assertNull(bodyMovementDetectionReference.get());
    }

    @Test
    public void test_observeBodyMovementDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideBodyMovementDetectionList();
        int masked = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", list.indexOf(optional.get()));
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetectionReference::set);

        assertEquals(optional.get().second, bodyMovementDetectionReference.get());
    }

    @Test
    public void test_observeBodyMovementDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideBodyMovementDetectionList();
        int masked = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetectionReference::set);
        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", list.indexOf(optional.get()));

        assertEquals(optional.get().second, bodyMovementDetectionReference.get());
    }

    @Test
    public void test_observeBodyMovementDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bodyMovementDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideBodyMovementDetectionList();
        int masked = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bodyMovementDetectionReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", list.indexOf(optional.get()));
        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", list.indexOf(optional.get()));

        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, bodyMovementDetectionReference.get());
    }

    @Test
    public void test_observeCuffFitDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);

        TestCase.assertNull(cuffFitDetectionReference.get());
    }

    @Test
    public void test_observeCuffFitDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideCuffFitDetectionList();
        int masked = MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", list.indexOf(optional.get()));
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);

        assertEquals(optional.get().second, cuffFitDetectionReference.get());
    }

    @Test
    public void test_observeCuffFitDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideCuffFitDetectionList();
        int masked = MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);
        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", list.indexOf(optional.get()));

        assertEquals(optional.get().second, cuffFitDetectionReference.get());
    }

    @Test
    public void test_observeCuffFitDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideCuffFitDetectionList();
        int masked = MEASUREMENT_STATUS_CUFF_FIT_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            cuffFitDetectionReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", list.indexOf(optional.get()));
        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", list.indexOf(optional.get()));

        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, cuffFitDetectionReference.get());
    }

    @Test
    public void test_observeIrregularPulseDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);

        TestCase.assertNull(cuffFitDetectionReference.get());
    }

    @Test
    public void test_observeIrregularPulseDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList();
        int masked = MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", list.indexOf(optional.get()));
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);

        assertEquals(optional.get().second, cuffFitDetectionReference.get());
    }

    @Test
    public void test_observeIrregularPulseDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList();
        int masked = MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), cuffFitDetectionReference::set);
        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", list.indexOf(optional.get()));

        assertEquals(optional.get().second, cuffFitDetectionReference.get());
    }

    @Test
    public void test_observeIrregularPulseDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> cuffFitDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList();
        int masked = MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            cuffFitDetectionReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", list.indexOf(optional.get()));
        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", list.indexOf(optional.get()));

        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, cuffFitDetectionReference.get());
    }

    @Test
    public void test_observePulseRateRangeDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();

        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetectionReference::set);

        TestCase.assertNull(pulseRateRangeDetectionReference.get());
    }

    @Test
    public void test_observePulseRateRangeDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList();
        int masked = MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", list.indexOf(optional.get()));
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetectionReference::set);

        assertEquals(optional.get().second, pulseRateRangeDetectionReference.get());
    }

    @Test
    public void test_observePulseRateRangeDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList();
        int masked = MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetectionReference::set);
        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", list.indexOf(optional.get()));

        assertEquals(optional.get().second, pulseRateRangeDetectionReference.get());
    }

    @Test
    public void test_observePulseRateRangeDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> pulseRateRangeDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList();
        int masked = MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            pulseRateRangeDetectionReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", list.indexOf(optional.get()));
        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", list.indexOf(optional.get()));

        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, pulseRateRangeDetectionReference.get());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();

        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetectionReference::set);

        TestCase.assertNull(measurementPositionDetectionReference.get());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList();
        int masked = MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", list.indexOf(optional.get()));
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetectionReference::set);

        assertEquals(optional.get().second, measurementPositionDetectionReference.get());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList();
        int masked = MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetectionReference::set);
        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", list.indexOf(optional.get()));

        assertEquals(optional.get().second, measurementPositionDetectionReference.get());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> measurementPositionDetectionReference = new AtomicReference<>();

        List<Pair<Integer, String>> list = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList();
        int masked = MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_MASK & original;
        Optional<Pair<Integer, String>> optional = list.stream().filter(integerStringPair -> integerStringPair.first == masked).findFirst();

        assertTrue(optional.isPresent());
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            measurementPositionDetectionReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", list.indexOf(optional.get()));
        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", list.indexOf(optional.get()));

        assertTrue(optional.isPresent());
        assertEquals(optional.get().second, measurementPositionDetectionReference.get());
    }

    @Test
    public void test_observeHasClientCharacteristicConfigurationDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();

        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), hasClientCharacteristicConfigurationDataJsonReference::set);

        TestCase.assertNull(hasClientCharacteristicConfigurationDataJsonReference.get());
    }

    @Test
    public void test_observeHasClientCharacteristicConfigurationDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON", original);
        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), hasClientCharacteristicConfigurationDataJsonReference::set);

        assertTrue(hasClientCharacteristicConfigurationDataJsonReference.get());
    }

    @Test
    public void test_observeHasClientCharacteristicConfigurationDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();

        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), hasClientCharacteristicConfigurationDataJsonReference::set);
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON", original);

        assertTrue(hasClientCharacteristicConfigurationDataJsonReference.get());
    }

    @Test
    public void test_observeHasClientCharacteristicConfigurationDataJson_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasClientCharacteristicConfigurationDataJsonReference = new AtomicReference<>();

        mViewModel.observeHasClientCharacteristicConfigurationDataJson(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasClientCharacteristicConfigurationDataJsonReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON", original);
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON", original);

        assertTrue(hasClientCharacteristicConfigurationDataJsonReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeClientCharacteristicConfiguration_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();

        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);

        TestCase.assertNull(clientCharacteristicConfigurationReference.get());
    }

    @Test
    public void test_observeClientCharacteristicConfiguration_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION", original);
        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);

        TestCase.assertEquals(original, clientCharacteristicConfigurationReference.get());
    }

    @Test
    public void test_observeClientCharacteristicConfiguration_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();

        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION", original);

        TestCase.assertEquals(original, clientCharacteristicConfigurationReference.get());
    }

    @Test
    public void test_observeClientCharacteristicConfiguration_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();

        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            clientCharacteristicConfigurationReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION", original);
        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION", original);

        TestCase.assertEquals(original, clientCharacteristicConfigurationReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeIndicationCount_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> indicationCountReference = new AtomicReference<>();

        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), indicationCountReference::set);

        TestCase.assertNull(indicationCountReference.get());
    }

    @Test
    public void test_observeIndicationCount_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> indicationCountReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INDICATION_COUNT", original);
        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), indicationCountReference::set);

        TestCase.assertEquals(original, indicationCountReference.get());
    }

    @Test
    public void test_observeIndicationCount_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> indicationCountReference = new AtomicReference<>();

        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), indicationCountReference::set);
        mSavedStateHandle.set("KEY_INDICATION_COUNT", original);

        TestCase.assertEquals(original, indicationCountReference.get());
    }

    @Test
    public void test_observeIndicationCount_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> indicationCountReference = new AtomicReference<>();

        mViewModel.observeIndicationCount(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            indicationCountReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INDICATION_COUNT", original);
        mSavedStateHandle.set("KEY_INDICATION_COUNT", original);

        TestCase.assertEquals(original, indicationCountReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeIndicationCountErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetIndicationCountErrorString = "a";
        AtomicReference<String> indicationCountErrorStringReference = new AtomicReference<>();

        mViewModel.observeIndicationCountErrorString(new TestLifeCycleOwner(), indicationCountErrorStringReference::set);

        TestCase.assertNull(indicationCountErrorStringReference.get());
    }

    @Test
    public void test_observeIndicationCountErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetIndicationCountErrorString = original;
        AtomicReference<String> indicationCountErrorStringReference = new AtomicReference<>();

        mViewModel.observeIndicationCountErrorString(new TestLifeCycleOwner(), indicationCountErrorStringReference::set);
        mViewModel.updateIndicationCount(null);

        TestCase.assertEquals(original, indicationCountErrorStringReference.get());
    }

    @Test
    public void test_observeIndicationCountErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetIndicationCountErrorString = original;
        AtomicReference<String> indicationCountErrorStringReference = new AtomicReference<>();

        mViewModel.observeIndicationCountErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            indicationCountErrorStringReference.set(s);
        });
        mViewModel.updateIndicationCount(null);
        mViewModel.updateIndicationCount(null);

        TestCase.assertEquals(original, indicationCountErrorStringReference.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_updateIsMmhg_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        TestCase.assertNull(mSavedStateHandle.get("KEY_IS_MMHG"));
        mViewModel.updateIsMmhg(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_MMHG").booleanValue());
    }

    @Test
    public void test_updateIsMmhg_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsMmhg(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_MMHG").booleanValue());

        mViewModel.updateIsMmhg(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_MMHG").booleanValue());
    }

    @Test
    public void test_updateSystolic_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        TestCase.assertNull(mSavedStateHandle.get("KEY_SYSTOLIC"));
        mViewModel.updateSystolic(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_SYSTOLIC"));
    }

    @Test
    public void test_updateSystolic_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updateSystolic(before);
        TestCase.assertEquals(before, mSavedStateHandle.get("KEY_SYSTOLIC"));

        mViewModel.updateSystolic(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_SYSTOLIC"));
    }

    @Test
    public void test_updateDiastolic_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        TestCase.assertNull(mSavedStateHandle.get("KEY_DIASTOLIC"));
        mViewModel.updateDiastolic(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_DIASTOLIC"));
    }

    @Test
    public void test_updateDiastolic_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updateDiastolic(before);
        TestCase.assertEquals(before, mSavedStateHandle.get("KEY_DIASTOLIC"));

        mViewModel.updateDiastolic(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_DIASTOLIC"));
    }

    @Test
    public void test_updateMeanArterialPressure_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        TestCase.assertNull(mSavedStateHandle.get("KEY_MEAN_ARTERIAL_PRESSURE"));
        mViewModel.updateMeanArterialPressure(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_MEAN_ARTERIAL_PRESSURE"));
    }

    @Test
    public void test_updateMeanArterialPressure_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updateMeanArterialPressure(before);
        TestCase.assertEquals(before, mSavedStateHandle.get("KEY_MEAN_ARTERIAL_PRESSURE"));

        mViewModel.updateMeanArterialPressure(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_MEAN_ARTERIAL_PRESSURE"));
    }

    @Test
    public void test_updateIsTimeStampSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        TestCase.assertNull(mSavedStateHandle.get("KEY_IS_TIME_STAMP_SUPPORTED"));
        mViewModel.updateIsTimeStampSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_TIME_STAMP_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateIsTimeStampSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsTimeStampSupported(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_TIME_STAMP_SUPPORTED").booleanValue());

        mViewModel.updateIsTimeStampSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_TIME_STAMP_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateTimeStampYear_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        TestCase.assertNull(mSavedStateHandle.get("KEY_TIME_STAMP_YEAR"));
        mViewModel.updateTimeStampYear(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_TIME_STAMP_YEAR"));
    }

    @Test
    public void test_updateTimeStampYear_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updateTimeStampYear(before);
        TestCase.assertEquals(before, mSavedStateHandle.get("KEY_TIME_STAMP_YEAR"));

        mViewModel.updateTimeStampYear(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_TIME_STAMP_YEAR"));
    }

    @Test
    public void test_updateTimeStampMonth_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_TIME_STAMP_MONTH"));
        mViewModel.updateTimeStampMonth(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_MONTH").intValue());
    }

    @Test
    public void test_updateTimeStampMonth_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateTimeStampMonth(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_MONTH").intValue());

        mViewModel.updateTimeStampMonth(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_MONTH").intValue());
    }

    @Test
    public void test_updateTimeStampDay_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_TIME_STAMP_DAY"));
        mViewModel.updateTimeStampDay(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_DAY").intValue());
    }

    @Test
    public void test_updateTimeStampDay_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateTimeStampDay(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_DAY").intValue());

        mViewModel.updateTimeStampDay(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_DAY").intValue());
    }

    @Test
    public void test_updateTimeStampHours_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_TIME_STAMP_HOURS"));
        mViewModel.updateTimeStampHours(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_HOURS").intValue());
    }

    @Test
    public void test_updateTimeStampHours_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateTimeStampHours(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_HOURS").intValue());

        mViewModel.updateTimeStampHours(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_HOURS").intValue());
    }

    @Test
    public void test_updateTimeStampMinutes_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_TIME_STAMP_MINUTES"));
        mViewModel.updateTimeStampMinutes(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_MINUTES").intValue());
    }

    @Test
    public void test_updateTimeStampMinutes_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateTimeStampMinutes(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_MINUTES").intValue());

        mViewModel.updateTimeStampMinutes(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_MINUTES").intValue());
    }

    @Test
    public void test_updateTimeStampSeconds_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_TIME_STAMP_SECONDS"));
        mViewModel.updateTimeStampSeconds(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_SECONDS").intValue());
    }

    @Test
    public void test_updateTimeStampSeconds_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateTimeStampSeconds(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_SECONDS").intValue());

        mViewModel.updateTimeStampSeconds(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_TIME_STAMP_SECONDS").intValue());
    }

    @Test
    public void test_updateIsPulseRateSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        TestCase.assertNull(mSavedStateHandle.get("KEY_IS_PULSE_RATE_SUPPORTED"));
        mViewModel.updateIsPulseRateSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_PULSE_RATE_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateIsPulseRateSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsPulseRateSupported(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_PULSE_RATE_SUPPORTED").booleanValue());

        mViewModel.updateIsPulseRateSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_PULSE_RATE_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updatePulseRate_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        TestCase.assertNull(mSavedStateHandle.get("KEY_PULSE_RATE"));
        mViewModel.updatePulseRate(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_PULSE_RATE"));
    }

    @Test
    public void test_updatePulseRate_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updatePulseRate(before);
        TestCase.assertEquals(before, mSavedStateHandle.get("KEY_PULSE_RATE"));

        mViewModel.updatePulseRate(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_PULSE_RATE"));
    }

    @Test
    public void test_updateIsUserIdSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        TestCase.assertNull(mSavedStateHandle.get("KEY_IS_USER_ID_SUPPORTED"));
        mViewModel.updateIsUserIdSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_USER_ID_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateIsUserIdSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsUserIdSupported(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_USER_ID_SUPPORTED").booleanValue());

        mViewModel.updateIsUserIdSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_USER_ID_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateUserId_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        TestCase.assertNull(mSavedStateHandle.get("KEY_USER_ID"));
        mViewModel.updateUserId(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_USER_ID"));
    }

    @Test
    public void test_updateUserId_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updateUserId(before);
        TestCase.assertEquals(before, mSavedStateHandle.get("KEY_USER_ID"));

        mViewModel.updateUserId(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_USER_ID"));
    }

    @Test
    public void test_updateIsMeasurementStatusSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        TestCase.assertNull(mSavedStateHandle.get("KEY_IS_MEASUREMENT_STATUS_SUPPORTED"));
        mViewModel.updateIsMeasurementStatusSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_MEASUREMENT_STATUS_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateIsMeasurementStatusSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsMeasurementStatusSupported(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_MEASUREMENT_STATUS_SUPPORTED").booleanValue());

        mViewModel.updateIsMeasurementStatusSupported(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_MEASUREMENT_STATUS_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateBodyMovementDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_BODY_MOVEMENT_DETECTION"));
        mViewModel.updateBodyMovementDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_BODY_MOVEMENT_DETECTION").intValue());
    }

    @Test
    public void test_updateBodyMovementDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateBodyMovementDetection(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_BODY_MOVEMENT_DETECTION").intValue());

        mViewModel.updateBodyMovementDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_BODY_MOVEMENT_DETECTION").intValue());
    }

    @Test
    public void test_updateCuffFitDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_CUFF_FIT_DETECTION"));
        mViewModel.updateCuffFitDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_CUFF_FIT_DETECTION").intValue());
    }

    @Test
    public void test_updateCuffFitDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateCuffFitDetection(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_CUFF_FIT_DETECTION").intValue());

        mViewModel.updateCuffFitDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_CUFF_FIT_DETECTION").intValue());
    }

    @Test
    public void test_updateIrregularPulseDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_IRREGULAR_PULSE_DETECTION"));
        mViewModel.updateIrregularPulseDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_IRREGULAR_PULSE_DETECTION").intValue());
    }

    @Test
    public void test_updateIrregularPulseDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateIrregularPulseDetection(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_IRREGULAR_PULSE_DETECTION").intValue());

        mViewModel.updateIrregularPulseDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_IRREGULAR_PULSE_DETECTION").intValue());
    }

    @Test
    public void test_updatePulseRateRangeDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_PULSE_RATE_RANGE_DETECTION"));
        mViewModel.updatePulseRateRangeDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_PULSE_RATE_RANGE_DETECTION").intValue());
    }

    @Test
    public void test_updatePulseRateRangeDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updatePulseRateRangeDetection(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_PULSE_RATE_RANGE_DETECTION").intValue());

        mViewModel.updatePulseRateRangeDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_PULSE_RATE_RANGE_DETECTION").intValue());
    }

    @Test
    public void test_updateMeasurementPositionDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int after = 2;

        TestCase.assertNull(mSavedStateHandle.get("KEY_MEASUREMENT_POSITION_DETECTION"));
        mViewModel.updateMeasurementPositionDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_MEASUREMENT_POSITION_DETECTION").intValue());
    }

    @Test
    public void test_updateMeasurementPositionDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int before = 1;
        int after = 2;

        mViewModel.updateMeasurementPositionDetection(before);
        TestCase.assertEquals(before, mSavedStateHandle.<Integer>get("KEY_MEASUREMENT_POSITION_DETECTION").intValue());

        mViewModel.updateMeasurementPositionDetection(after);

        TestCase.assertEquals(after, mSavedStateHandle.<Integer>get("KEY_MEASUREMENT_POSITION_DETECTION").intValue());
    }

    @Test
    public void test_updateIndicationCount_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        TestCase.assertNull(mSavedStateHandle.get("KEY_INDICATION_COUNT"));
        mViewModel.updateIndicationCount(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_INDICATION_COUNT"));
    }

    @Test
    public void test_updateIndicationCount_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updateIndicationCount(before);
        TestCase.assertEquals(before, mSavedStateHandle.get("KEY_INDICATION_COUNT"));

        mViewModel.updateIndicationCount(after);

        TestCase.assertEquals(after, mSavedStateHandle.get("KEY_INDICATION_COUNT"));
    }

    @Test
    public void test_getClientCharacteristicConfigurationDescriptorJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        TestCase.assertNull(mViewModel.getClientCharacteristicConfigurationDescriptorJson());
    }

    @Test
    public void test_getClientCharacteristicConfigurationDescriptorJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";

        mSavedStateHandle.set("KEY_CLIENT_CHARACTERISTIC_CONFIGURATION_DATA_JSON", original);
        TestCase.assertEquals(original, mViewModel.getClientCharacteristicConfigurationDescriptorJson());
    }

    @Test
    public void test_setClientCharacteristicConfigurationDescriptorJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();

        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        String originalJson = mGson.toJson(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(originalJson);

        TestCase.assertEquals(originalJson, mViewModel.getClientCharacteristicConfigurationDescriptorJson());
        TestCase.assertEquals(mFakeDeviceSettingRepository.getIndicationsString(new ClientCharacteristicConfiguration(clientCharacteristicConfigurationDescriptorData.data).isPropertiesIndicationsEnabled())
                , clientCharacteristicConfigurationReference.get());
    }

    @Test
    public void test_setClientCharacteristicConfigurationDescriptorJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();

        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);

        Intent intent = new Intent();
        DescriptorData descriptorData = new DescriptorData();
        descriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        descriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        descriptorData.data = BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE;
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), mGson.toJson(descriptorData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        DescriptorData clientCharacteristicConfigurationDescriptorData = new DescriptorData();
        clientCharacteristicConfigurationDescriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        clientCharacteristicConfigurationDescriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        clientCharacteristicConfigurationDescriptorData.data = BluetoothGattDescriptor.ENABLE_INDICATION_VALUE;
        String originalJson = mGson.toJson(clientCharacteristicConfigurationDescriptorData);
        mViewModel.setClientCharacteristicConfigurationDescriptorJson(originalJson);

        TestCase.assertEquals(originalJson, mViewModel.getClientCharacteristicConfigurationDescriptorJson());
        TestCase.assertEquals(mFakeDeviceSettingRepository.getIndicationsString(new ClientCharacteristicConfiguration(clientCharacteristicConfigurationDescriptorData.data).isPropertiesIndicationsEnabled())
                , clientCharacteristicConfigurationReference.get());
    }

    @Test
    public void test_setClientCharacteristicConfigurationDescriptorJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> clientCharacteristicConfigurationReference = new AtomicReference<>();

        mViewModel.observeClientCharacteristicConfiguration(new TestLifeCycleOwner(), clientCharacteristicConfigurationReference::set);

        Intent intent = new Intent();
        DescriptorData descriptorData = new DescriptorData();
        descriptorData.uuid = CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR;
        descriptorData.permission = BluetoothGattDescriptor.PERMISSION_READ | BluetoothGattDescriptor.PERMISSION_WRITE;
        descriptorData.data = BluetoothGattDescriptor.DISABLE_NOTIFICATION_VALUE;
        intent.putExtra(CLIENT_CHARACTERISTIC_CONFIGURATION_DESCRIPTOR.toString(), mGson.toJson(descriptorData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setClientCharacteristicConfigurationDescriptorJson(null);

        TestCase.assertNull(mViewModel.getClientCharacteristicConfigurationDescriptorJson());
        assertEquals("", clientCharacteristicConfigurationReference.get());
    }

    @Test
    public void test_provideDateTimeMonthList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.provideDateTimeMonthList();
        TestCase.assertEquals(original, mViewModel.provideDateTimeMonthList());
    }

    @Test
    public void test_provideDateTimeDayList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.provideDateTimeDayList();
        TestCase.assertEquals(original, mViewModel.provideDateTimeDayList());
    }

    @Test
    public void test_provideDateTimeHoursList_00001() {
        List<String> original = mFakeDeviceSettingRepository.provideDateTimeHoursList();
        TestCase.assertEquals(original, mViewModel.provideDateTimeHoursList());
    }

    @Test
    public void test_provideDateTimeMinutesList_00001() {
        List<String> original = mFakeDeviceSettingRepository.provideDateTimeMinutesList();
        TestCase.assertEquals(original, mViewModel.provideDateTimeMinutesList());
    }

    @Test
    public void test_provideDateTimeSecondsList_00001() {
        List<String> original = mFakeDeviceSettingRepository.provideDateTimeSecondsList();
        TestCase.assertEquals(original, mViewModel.provideDateTimeSecondsList());
    }

    @Test
    public void test_provideBodyMovementDetectionList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.provideBodyMovementDetectionList();
        TestCase.assertEquals(original, mViewModel.provideBodyMovementDetectionList());
    }

    @Test
    public void test_provideCuffFitDetectionList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.provideCuffFitDetectionList();
        TestCase.assertEquals(original, mViewModel.provideCuffFitDetectionList());
    }

    @Test
    public void test_provideIrregularPulseDetectionList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.provideIrregularPulseDetectionList();
        TestCase.assertEquals(original, mViewModel.provideIrregularPulseDetectionList());
    }

    @Test
    public void test_providePulseRateRangeDetectionList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.providePulseRateRangeDetectionList();
        TestCase.assertEquals(original, mViewModel.providePulseRateRangeDetectionList());
    }

    @Test
    public void test_provideMeasurementPositionDetectionList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.provideMeasurementPositionDetectionList();
        TestCase.assertEquals(original, mViewModel.provideMeasurementPositionDetectionList());
    }

}


