package org.im97mori.ble.android.peripheral.ui.device.setting.u2a49;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;

import android.bluetooth.BluetoothGatt;
import android.bluetooth.BluetoothGattCharacteristic;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.LinkedList;
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

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class BloodPressureFeatureSettingViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private BloodPressureFeatureSettingViewModel mViewModel;

    private SavedStateHandle mSavedStateHandle;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new BloodPressureFeatureSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository);
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

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertFalse(isBodyMovementDetectionSupportedReference.get());
        assertFalse(isCuffFitDetectionSupportSupportedReference.get());
        assertFalse(isIrregularPulseDetectionSupported.get());
        assertFalse(isPulseRateRangeDetectionSupported.get());
        assertFalse(isMeasurementPositionDetectionSupportedReference.get());
        assertFalse(isMultipleBondSupportedReference.get());
        assertEquals("0", responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals("0", responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , 2
                , 1
                , null
                , -1);
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(isErrorResponseReference.get());
        assertFalse(isBodyMovementDetectionSupportedReference.get());
        assertFalse(isCuffFitDetectionSupportSupportedReference.get());
        assertFalse(isIrregularPulseDetectionSupported.get());
        assertFalse(isPulseRateRangeDetectionSupported.get());
        assertFalse(isMeasurementPositionDetectionSupportedReference.get());
        assertFalse(isMultipleBondSupportedReference.get());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertNull(responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = false;
        characteristicData.data = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false).getBytes();
        characteristicData.delay = 1;
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertEquals(isBodyMovementDetectionSupported, isBodyMovementDetectionSupportedReference.get().booleanValue());
        assertEquals(isCuffFitDetectionSupportSupported, isCuffFitDetectionSupportSupportedReference.get().booleanValue());
        assertEquals(hasIrregularPulseDetection, isIrregularPulseDetectionSupported.get().booleanValue());
        assertEquals(hasPulseRateRangeDetection, isPulseRateRangeDetectionSupported.get().booleanValue());
        assertEquals(isMeasurementPositionDetectionSupported, isMeasurementPositionDetectionSupportedReference.get().booleanValue());
        assertEquals(isMultipleBondSupported, isMultipleBondSupportedReference.get().booleanValue());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = true;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = false;
        characteristicData.data = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false).getBytes();
        characteristicData.delay = 1;
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertEquals(isBodyMovementDetectionSupported, isBodyMovementDetectionSupportedReference.get().booleanValue());
        assertEquals(isCuffFitDetectionSupportSupported, isCuffFitDetectionSupportSupportedReference.get().booleanValue());
        assertEquals(hasIrregularPulseDetection, isIrregularPulseDetectionSupported.get().booleanValue());
        assertEquals(hasPulseRateRangeDetection, isPulseRateRangeDetectionSupported.get().booleanValue());
        assertEquals(isMeasurementPositionDetectionSupported, isMeasurementPositionDetectionSupportedReference.get().booleanValue());
        assertEquals(isMultipleBondSupported, isMultipleBondSupportedReference.get().booleanValue());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = true;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = false;
        characteristicData.data = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false).getBytes();
        characteristicData.delay = 1;
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertEquals(isBodyMovementDetectionSupported, isBodyMovementDetectionSupportedReference.get().booleanValue());
        assertEquals(isCuffFitDetectionSupportSupported, isCuffFitDetectionSupportSupportedReference.get().booleanValue());
        assertEquals(hasIrregularPulseDetection, isIrregularPulseDetectionSupported.get().booleanValue());
        assertEquals(hasPulseRateRangeDetection, isPulseRateRangeDetectionSupported.get().booleanValue());
        assertEquals(isMeasurementPositionDetectionSupported, isMeasurementPositionDetectionSupportedReference.get().booleanValue());
        assertEquals(isMultipleBondSupported, isMultipleBondSupportedReference.get().booleanValue());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = true;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = false;
        characteristicData.data = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false).getBytes();
        characteristicData.delay = 1;
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertEquals(isBodyMovementDetectionSupported, isBodyMovementDetectionSupportedReference.get().booleanValue());
        assertEquals(isCuffFitDetectionSupportSupported, isCuffFitDetectionSupportSupportedReference.get().booleanValue());
        assertEquals(hasIrregularPulseDetection, isIrregularPulseDetectionSupported.get().booleanValue());
        assertEquals(hasPulseRateRangeDetection, isPulseRateRangeDetectionSupported.get().booleanValue());
        assertEquals(isMeasurementPositionDetectionSupported, isMeasurementPositionDetectionSupportedReference.get().booleanValue());
        assertEquals(isMultipleBondSupported, isMultipleBondSupportedReference.get().booleanValue());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = true;
        boolean isMultipleBondSupported = false;
        characteristicData.data = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false).getBytes();
        characteristicData.delay = 1;
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertEquals(isBodyMovementDetectionSupported, isBodyMovementDetectionSupportedReference.get().booleanValue());
        assertEquals(isCuffFitDetectionSupportSupported, isCuffFitDetectionSupportSupportedReference.get().booleanValue());
        assertEquals(hasIrregularPulseDetection, isIrregularPulseDetectionSupported.get().booleanValue());
        assertEquals(hasPulseRateRangeDetection, isPulseRateRangeDetectionSupported.get().booleanValue());
        assertEquals(isMeasurementPositionDetectionSupported, isMeasurementPositionDetectionSupportedReference.get().booleanValue());
        assertEquals(isMultipleBondSupported, isMultipleBondSupportedReference.get().booleanValue());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeSetup_3_00006() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();
        AtomicReference<Boolean> isBodyMovementDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isCuffFitDetectionSupportSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isIrregularPulseDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isPulseRateRangeDetectionSupported = new AtomicReference<>();
        AtomicReference<Boolean> isMeasurementPositionDetectionSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> isMultipleBondSupportedReference = new AtomicReference<>();
        AtomicReference<String> responseCodeReference = new AtomicReference<>();
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();
        AtomicReference<String> responseDelayReference = new AtomicReference<>();
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), isBodyMovementDetectionSupportedReference::set);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), isCuffFitDetectionSupportSupportedReference::set);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), isIrregularPulseDetectionSupported::set);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), isPulseRateRangeDetectionSupported::set);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), isMeasurementPositionDetectionSupportedReference::set);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), isMultipleBondSupportedReference::set);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        Intent intent = new Intent();
        CharacteristicData characteristicData = new CharacteristicData(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC
                , BluetoothGattCharacteristic.PROPERTY_READ
                , BluetoothGattCharacteristic.PERMISSION_READ
                , new LinkedList<>()
                , BluetoothGatt.GATT_SUCCESS
                , 0
                , null
                , -1);
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = true;
        characteristicData.data = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false).getBytes();
        characteristicData.delay = 1;
        intent.putExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString(), Utils.parcelableToByteArray(characteristicData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertFalse(isErrorResponseReference.get());
        assertEquals(isBodyMovementDetectionSupported, isBodyMovementDetectionSupportedReference.get().booleanValue());
        assertEquals(isCuffFitDetectionSupportSupported, isCuffFitDetectionSupportSupportedReference.get().booleanValue());
        assertEquals(hasIrregularPulseDetection, isIrregularPulseDetectionSupported.get().booleanValue());
        assertEquals(hasPulseRateRangeDetection, isPulseRateRangeDetectionSupported.get().booleanValue());
        assertEquals(isMeasurementPositionDetectionSupported, isMeasurementPositionDetectionSupportedReference.get().booleanValue());
        assertEquals(isMultipleBondSupported, isMultipleBondSupportedReference.get().booleanValue());
        assertEquals(String.valueOf(characteristicData.responseCode), responseCodeReference.get());
        assertEquals(mContext.getString(R.string.out_of_range), responseCodeErrorStringReference.get());
        assertEquals(String.valueOf(characteristicData.delay), responseDelayReference.get());
        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeIsErrorResponse_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);

        assertNull(isErrorResponseReference.get());
    }

    @Test
    public void test_observeIsErrorResponse_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);

        assertEquals(original, isErrorResponseReference.get().booleanValue());
    }

    @Test
    public void test_observeIsErrorResponse_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);
        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), isErrorResponseReference::set);

        assertEquals(original, isErrorResponseReference.get().booleanValue());
    }

    @Test
    public void test_observeIsErrorResponse_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isErrorResponseReference = new AtomicReference<>();

        mViewModel.observeIsErrorResponse(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isErrorResponseReference.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);
        mSavedStateHandle.set("KEY_IS_ERROR_RESPONSE", original);

        assertEquals(original, isErrorResponseReference.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeResponseCode_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);

        assertNull(responseCodeReference.get());
    }

    @Test
    public void test_observeResponseCode_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);
        mViewModel.updateResponseCode(original);

        assertEquals(original, responseCodeReference.get());
    }

    @Test
    public void test_observeResponseCode_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.updateResponseCode(original);
        mViewModel.observeResponseCode(new TestLifeCycleOwner(), responseCodeReference::set);

        assertEquals(original, responseCodeReference.get());
    }

    @Test
    public void test_observeResponseCode_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> responseCodeReference = new AtomicReference<>();

        mViewModel.observeResponseCode(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            responseCodeReference.set(aBoolean);
        });
        mViewModel.updateResponseCode(original);
        mViewModel.updateResponseCode(original);

        assertEquals(original, responseCodeReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeResponseCodeErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = "a";
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);

        assertNull(responseCodeErrorStringReference.get());
    }

    @Test
    public void test_observeResponseCodeErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = original;
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), responseCodeErrorStringReference::set);
        mViewModel.updateResponseCode("");

        assertEquals(original, responseCodeErrorStringReference.get());
    }

    @Test
    public void test_observeResponseCodeErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = original;
        AtomicReference<String> responseCodeErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseCodeErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            responseCodeErrorStringReference.set(s);
        });
        mViewModel.updateResponseCode("");
        mViewModel.updateResponseCode("");

        assertEquals(original, responseCodeErrorStringReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeResponseDelay_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);

        assertNull(responseDelayReference.get());
    }

    @Test
    public void test_observeResponseDelay_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);
        mViewModel.updateResponseDelay(original);

        assertEquals(original, responseDelayReference.get());
    }

    @Test
    public void test_observeResponseDelay_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.updateResponseDelay(original);
        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), responseDelayReference::set);

        assertEquals(original, responseDelayReference.get());
    }

    @Test
    public void test_observeResponseDelay_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "1";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> responseDelayReference = new AtomicReference<>();

        mViewModel.observeResponseDelay(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            responseDelayReference.set(aBoolean);
        });
        mViewModel.updateResponseDelay(original);
        mViewModel.updateResponseDelay(original);

        assertEquals(original, responseDelayReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeResponseDelayErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetResponseCodeErrorString = "a";
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);

        assertNull(responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeResponseDelayErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetResponseDelayErrorString = original;
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), responseDelayErrorStringReference::set);
        mViewModel.updateResponseDelay("");

        assertEquals(original, responseDelayErrorStringReference.get());
    }

    @Test
    public void test_observeResponseDelayErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetResponseDelayErrorString = original;
        AtomicReference<String> responseDelayErrorStringReference = new AtomicReference<>();

        mViewModel.observeResponseDelayErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            responseDelayErrorStringReference.set(s);
        });
        mViewModel.updateResponseDelay("");
        mViewModel.updateResponseDelay("");

        assertEquals(original, responseDelayErrorStringReference.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_updateIsErrorResponse_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_IS_ERROR_RESPONSE"));
        mViewModel.updateIsErrorResponse(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE"));
    }

    @Test
    public void test_updateIsErrorResponse_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateIsErrorResponse(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE"));

        mViewModel.updateIsErrorResponse(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_ERROR_RESPONSE"));
    }

    @Test
    public void test_updateResponseCode_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "1";

        assertNull(mSavedStateHandle.get("KEY_RESPONSE_CODE"));
        mViewModel.updateResponseCode(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_CODE"));
    }

    @Test
    public void test_updateResponseCode_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "1";
        String after = "2";

        mViewModel.updateResponseCode(before);
        assertEquals(before, mSavedStateHandle.get("KEY_RESPONSE_CODE"));

        mViewModel.updateResponseCode(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_CODE"));
    }

    @Test
    public void test_updateResponseDelay_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "1";

        assertNull(mSavedStateHandle.get("KEY_RESPONSE_DELAY"));
        mViewModel.updateResponseDelay(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_DELAY"));
    }

    @Test
    public void test_updateResponseDelay_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "1";
        String after = "2";

        mViewModel.updateResponseDelay(before);
        assertEquals(before, mSavedStateHandle.get("KEY_RESPONSE_DELAY"));

        mViewModel.updateResponseDelay(after);

        assertEquals(after, mSavedStateHandle.get("KEY_RESPONSE_DELAY"));
    }

    @Test
    public void test_observeSaveData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Intent> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);

        assertNull(saveDataReference.get());
    }

    @Test
    public void test_observeSaveData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent original = new Intent();
        AtomicReference<Intent> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);
        mSavedStateHandle.set("KEY_SAVED_DATA", original);

        assertEquals(original, saveDataReference.get());
    }

    @Test
    public void test_save_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Already saved", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay("");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateIsErrorResponse(true);
        mViewModel.updateResponseCode("");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int responseCode = 1;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(true);
        mViewModel.updateResponseCode(String.valueOf(responseCode));

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                characteristicDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()), CharacteristicData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(responseCode, characteristicData.responseCode);
    }

    @Test
    public void test_save_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean bodyMovementDetection = true;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateBodyMovementDetection(bodyMovementDetection);

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                characteristicDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()), CharacteristicData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, characteristicData.responseCode);
        assertEquals(bodyMovementDetection, new BloodPressureFeature(characteristicData.data).isBodyMovementDetectionSupported());
    }

    @Test
    public void test_save_3_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean cuffFitDetection = true;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateCuffFitDetection(cuffFitDetection);

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                characteristicDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()), CharacteristicData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, characteristicData.responseCode);
        assertEquals(cuffFitDetection, new BloodPressureFeature(characteristicData.data).isCuffFitDetectionSupported());
    }

    @Test
    public void test_save_3_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean irregularPulseDetection = true;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateIrregularPulseDetection(irregularPulseDetection);

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                characteristicDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()), CharacteristicData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, characteristicData.responseCode);
        assertEquals(irregularPulseDetection, new BloodPressureFeature(characteristicData.data).isIrregularPulseDetectionSupported());
    }

    @Test
    public void test_save_3_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean pulseRateRangeDetection = true;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updatePulseRateRangeDetection(pulseRateRangeDetection);

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                characteristicDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()), CharacteristicData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, characteristicData.responseCode);
        assertEquals(pulseRateRangeDetection, new BloodPressureFeature(characteristicData.data).isPulseRateRangeDetectionSupported());
    }

    @Test
    public void test_save_3_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean measurementPositionDetection = true;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateMeasurementPositionDetection(measurementPositionDetection);

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                characteristicDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()), CharacteristicData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, characteristicData.responseCode);
        assertEquals(measurementPositionDetection, new BloodPressureFeature(characteristicData.data).isMeasurementPositionDetectionSupported());
    }

    @Test
    public void test_save_3_00006() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean multipleBondDetection = true;
        long delay = 2;
        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateResponseDelay(String.valueOf(delay));
        mViewModel.updateIsErrorResponse(false);
        mViewModel.updateMultipleBond(multipleBondDetection);

        AtomicReference<CharacteristicData> characteristicDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), resultIntent ->
                characteristicDataAtomicReference.set(Utils.byteToParcelable(resultIntent.getByteArrayExtra(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC.toString()), CharacteristicData.CREATOR)));
        mViewModel.save(throwable -> {
        });

        CharacteristicData characteristicData = characteristicDataAtomicReference.get();
        assertNotNull(characteristicData);
        assertEquals(delay, characteristicData.delay);
        assertEquals(BluetoothGatt.GATT_SUCCESS, characteristicData.responseCode);
        assertEquals(multipleBondDetection, new BloodPressureFeature(characteristicData.data).isMultipleBondSupported());
    }

    @Test
    public void test_observeBodyMovementDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> bodyMovementDetection = new AtomicReference<>();

        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetection::set);

        assertNull(bodyMovementDetection.get());
    }

    @Test
    public void test_observeBodyMovementDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> bodyMovementDetection = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", original);
        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetection::set);

        assertEquals(original, bodyMovementDetection.get().booleanValue());
    }

    @Test
    public void test_observeBodyMovementDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> bodyMovementDetection = new AtomicReference<>();

        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), bodyMovementDetection::set);
        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", original);

        assertEquals(original, bodyMovementDetection.get().booleanValue());
    }

    @Test
    public void test_observeBodyMovementDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> bodyMovementDetection = new AtomicReference<>();

        mViewModel.observeBodyMovementDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bodyMovementDetection.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", original);
        mSavedStateHandle.set("KEY_BODY_MOVEMENT_DETECTION", original);

        assertEquals(original, bodyMovementDetection.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeCuffFitDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> cuffFitDetection = new AtomicReference<>();

        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetection::set);

        assertNull(cuffFitDetection.get());
    }

    @Test
    public void test_observeCuffFitDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> cuffFitDetection = new AtomicReference<>();

        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", original);
        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetection::set);

        assertEquals(original, cuffFitDetection.get().booleanValue());
    }

    @Test
    public void test_observeCuffFitDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> cuffFitDetection = new AtomicReference<>();

        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), cuffFitDetection::set);
        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", original);

        assertEquals(original, cuffFitDetection.get().booleanValue());
    }

    @Test
    public void test_observeCuffFitDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> cuffFitDetection = new AtomicReference<>();

        mViewModel.observeCuffFitDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            cuffFitDetection.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", original);
        mSavedStateHandle.set("KEY_CUFF_FIT_DETECTION", original);

        assertEquals(original, cuffFitDetection.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIrregularPulseDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> irregularPulseDetection = new AtomicReference<>();

        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), irregularPulseDetection::set);

        assertNull(irregularPulseDetection.get());
    }

    @Test
    public void test_observeIrregularPulseDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> irregularPulseDetection = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", original);
        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), irregularPulseDetection::set);

        assertEquals(original, irregularPulseDetection.get().booleanValue());
    }

    @Test
    public void test_observeIrregularPulseDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> irregularPulseDetection = new AtomicReference<>();

        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), irregularPulseDetection::set);
        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", original);

        assertEquals(original, irregularPulseDetection.get().booleanValue());
    }

    @Test
    public void test_observeIrregularPulseDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> irregularPulseDetection = new AtomicReference<>();

        mViewModel.observeIrregularPulseDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            irregularPulseDetection.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", original);
        mSavedStateHandle.set("KEY_IRREGULAR_PULSE_DETECTION", original);

        assertEquals(original, irregularPulseDetection.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observePulseRateRangeDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> pulseRateRangeDetection = new AtomicReference<>();

        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetection::set);

        assertNull(pulseRateRangeDetection.get());
    }

    @Test
    public void test_observePulseRateRangeDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> pulseRateRangeDetection = new AtomicReference<>();

        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", original);
        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetection::set);

        assertEquals(original, pulseRateRangeDetection.get().booleanValue());
    }

    @Test
    public void test_observePulseRateRangeDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> pulseRateRangeDetection = new AtomicReference<>();

        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), pulseRateRangeDetection::set);
        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", original);

        assertEquals(original, pulseRateRangeDetection.get().booleanValue());
    }

    @Test
    public void test_observePulseRateRangeDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> pulseRateRangeDetection = new AtomicReference<>();

        mViewModel.observePulseRateRangeDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            pulseRateRangeDetection.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", original);
        mSavedStateHandle.set("KEY_PULSE_RATE_RANGE_DETECTION", original);

        assertEquals(original, pulseRateRangeDetection.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> measurementPositionDetection = new AtomicReference<>();

        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetection::set);

        assertNull(measurementPositionDetection.get());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> measurementPositionDetection = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", original);
        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetection::set);

        assertEquals(original, measurementPositionDetection.get().booleanValue());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> measurementPositionDetection = new AtomicReference<>();

        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), measurementPositionDetection::set);
        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", original);

        assertEquals(original, measurementPositionDetection.get().booleanValue());
    }

    @Test
    public void test_observeMeasurementPositionDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> measurementPositionDetection = new AtomicReference<>();

        mViewModel.observeMeasurementPositionDetection(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            measurementPositionDetection.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", original);
        mSavedStateHandle.set("KEY_MEASUREMENT_POSITION_DETECTION", original);

        assertEquals(original, measurementPositionDetection.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeMultipleBondDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> multipleBondDetection = new AtomicReference<>();

        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), multipleBondDetection::set);

        assertNull(multipleBondDetection.get());
    }

    @Test
    public void test_observeMultipleBondDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> multipleBondDetection = new AtomicReference<>();

        mSavedStateHandle.set("KEY_MULTIPLE_BOND", original);
        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), multipleBondDetection::set);

        assertEquals(original, multipleBondDetection.get().booleanValue());
    }

    @Test
    public void test_observeMultipleBondDetection_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> multipleBondDetection = new AtomicReference<>();

        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), multipleBondDetection::set);
        mSavedStateHandle.set("KEY_MULTIPLE_BOND", original);

        assertEquals(original, multipleBondDetection.get().booleanValue());
    }

    @Test
    public void test_observeMultipleBondDetection_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> multipleBondDetection = new AtomicReference<>();

        mViewModel.observeMultipleBond(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            multipleBondDetection.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_MULTIPLE_BOND", original);
        mSavedStateHandle.set("KEY_MULTIPLE_BOND", original);

        assertEquals(original, multipleBondDetection.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_updateBodyMovementDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_BODY_MOVEMENT_DETECTION"));
        mViewModel.updateBodyMovementDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_BODY_MOVEMENT_DETECTION"));
    }

    @Test
    public void test_updateBodyMovementDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateBodyMovementDetection(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_BODY_MOVEMENT_DETECTION"));

        mViewModel.updateBodyMovementDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_BODY_MOVEMENT_DETECTION"));
    }

    @Test
    public void test_updateCuffFitDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_CUFF_FIT_DETECTION"));
        mViewModel.updateCuffFitDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_CUFF_FIT_DETECTION"));
    }

    @Test
    public void test_updateCuffFitDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateCuffFitDetection(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_CUFF_FIT_DETECTION"));

        mViewModel.updateCuffFitDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_CUFF_FIT_DETECTION"));
    }

    @Test
    public void test_updateIrregularPulseDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_IRREGULAR_PULSE_DETECTION"));
        mViewModel.updateIrregularPulseDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IRREGULAR_PULSE_DETECTION"));
    }

    @Test
    public void test_updateIrregularPulseDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateIrregularPulseDetection(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IRREGULAR_PULSE_DETECTION"));

        mViewModel.updateIrregularPulseDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IRREGULAR_PULSE_DETECTION"));
    }

    @Test
    public void test_updatePulseRateRangeDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_PULSE_RATE_RANGE_DETECTION"));
        mViewModel.updatePulseRateRangeDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_PULSE_RATE_RANGE_DETECTION"));
    }

    @Test
    public void test_updatePulseRateRangeDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updatePulseRateRangeDetection(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_PULSE_RATE_RANGE_DETECTION"));

        mViewModel.updatePulseRateRangeDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_PULSE_RATE_RANGE_DETECTION"));
    }

    @Test
    public void test_updateMeasurementPositionDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_MEASUREMENT_POSITION_DETECTION"));
        mViewModel.updateMeasurementPositionDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_MEASUREMENT_POSITION_DETECTION"));
    }

    @Test
    public void test_updateMeasurementPositionDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateMeasurementPositionDetection(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_MEASUREMENT_POSITION_DETECTION"));

        mViewModel.updateMeasurementPositionDetection(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_MEASUREMENT_POSITION_DETECTION"));
    }

    @Test
    public void test_updateMultipleBondDetection_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean after = Boolean.TRUE;

        assertNull(mSavedStateHandle.get("KEY_MULTIPLE_BOND"));
        mViewModel.updateMultipleBond(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_MULTIPLE_BOND"));
    }

    @Test
    public void test_updateMultipleBondDetection_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Boolean before = Boolean.FALSE;
        Boolean after = Boolean.TRUE;

        mViewModel.updateMultipleBond(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_MULTIPLE_BOND"));

        mViewModel.updateMultipleBond(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_MULTIPLE_BOND"));
    }

}