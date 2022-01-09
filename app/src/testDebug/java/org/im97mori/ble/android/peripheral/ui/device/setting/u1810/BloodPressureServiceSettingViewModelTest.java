package org.im97mori.ble.android.peripheral.ui.device.setting.u1810;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.junit.Assert.assertNull;

import android.bluetooth.BluetoothGattCharacteristic;
import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import junit.framework.TestCase;

import org.im97mori.ble.BLEUtils;
import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.characteristic.u2a35.BloodPressureMeasurement;
import org.im97mori.ble.characteristic.u2a36.IntermediateCuffPressure;
import org.im97mori.ble.characteristic.u2a49.BloodPressureFeature;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

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
public class BloodPressureServiceSettingViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private BloodPressureServiceSettingViewModel mViewModel;

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
        mViewModel = new BloodPressureServiceSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository, mGson);
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

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasBloodPressureFeatureDataJsonReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureMeasurementFlagsReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementSystolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementDiastolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeanArterialPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> bloodPressureMeasurementPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementPulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> intermediateCuffPressureFlagsReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> intermediateCuffPressurePulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressurePulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureFeatureReference = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJsonReference::set);
        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureSupportedReference::set);
        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJsonReference::set);
        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJsonReference::set);

        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlagsReference::set);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolicReference::set);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolicReference::set);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressureReference::set);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStampReference::set);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateReference::set);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementUserIdSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserIdReference::set);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatusReference::set);

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlagsReference::set);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressureReference::set);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStampReference::set);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateSupportedReference::set);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateReference::set);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserIdReference::set);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatusReference::set);

        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), bloodPressureFeatureReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        TestCase.assertNull(hasBloodPressureMeasurementDataJsonReference.get());
        assertFalse(isIntermediateCuffPressureSupportedReference.get());
        TestCase.assertNull(hasIntermediateCuffPressureDataJsonReference.get());
        TestCase.assertNull(hasBloodPressureFeatureDataJsonReference.get());

        assertEquals("", bloodPressureMeasurementFlagsReference.get());
        assertEquals("", bloodPressureMeasurementSystolicReference.get());
        assertEquals("", bloodPressureMeasurementDiastolicReference.get());
        assertEquals("", bloodPressureMeanArterialPressureReference.get());
        assertFalse(isBloodPressureMeasurementTimeStampSupportedReference.get());
        assertEquals("", bloodPressureMeasurementTimeStampReference.get());
        assertFalse(bloodPressureMeasurementPulseRateSupportedReference.get());
        assertEquals("", bloodPressureMeasurementPulseRateReference.get());
        assertFalse(isBloodPressureMeasurementUserIdSupportedReference.get());
        assertEquals("", bloodPressureMeasurementUserIdReference.get());
        assertFalse(isBloodPressureMeasurementMeasurementStatusSupportedReference.get());
        assertEquals("", bloodPressureMeasurementMeasurementStatusReference.get());

        assertEquals("", intermediateCuffPressureFlagsReference.get());
        assertEquals("", intermediateCuffPressureCurrentCuffPressureReference.get());
        assertFalse(isIntermediateCuffPressureTimeStampSupportedReference.get());
        assertEquals("", intermediateCuffPressureTimeStampReference.get());
        assertFalse(intermediateCuffPressurePulseRateSupportedReference.get());
        assertEquals("", intermediateCuffPressurePulseRateReference.get());
        assertFalse(isIntermediateCuffPressureUserIdSupportedReference.get());
        assertEquals("", intermediateCuffPressureUserIdReference.get());
        assertFalse(isIntermediateCuffPressureMeasurementStatusSupportedReference.get());
        assertEquals("", intermediateCuffPressureMeasurementStatusReference.get());

        assertEquals("", bloodPressureFeatureReference.get());
    }

    @Test
    public void test_observeSetup_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasBloodPressureFeatureDataJsonReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureMeasurementFlagsReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementSystolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementDiastolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeanArterialPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> bloodPressureMeasurementPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementPulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> intermediateCuffPressureFlagsReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> intermediateCuffPressurePulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressurePulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureFeatureReference = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJsonReference::set);
        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureSupportedReference::set);
        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJsonReference::set);
        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJsonReference::set);
        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlagsReference::set);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolicReference::set);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolicReference::set);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressureReference::set);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStampReference::set);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateReference::set);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementUserIdSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserIdReference::set);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatusReference::set);

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlagsReference::set);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressureReference::set);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStampReference::set);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateSupportedReference::set);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateReference::set);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserIdReference::set);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatusReference::set);

        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), bloodPressureFeatureReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = BLOOD_PRESSURE_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;
        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        TestCase.assertNull(hasBloodPressureMeasurementDataJsonReference.get());
        assertFalse(isIntermediateCuffPressureSupportedReference.get());
        TestCase.assertNull(hasIntermediateCuffPressureDataJsonReference.get());
        TestCase.assertNull(hasBloodPressureFeatureDataJsonReference.get());

        assertEquals("", bloodPressureMeasurementFlagsReference.get());
        assertEquals("", bloodPressureMeasurementSystolicReference.get());
        assertEquals("", bloodPressureMeasurementDiastolicReference.get());
        assertEquals("", bloodPressureMeanArterialPressureReference.get());
        assertFalse(isBloodPressureMeasurementTimeStampSupportedReference.get());
        assertEquals("", bloodPressureMeasurementTimeStampReference.get());
        assertFalse(bloodPressureMeasurementPulseRateSupportedReference.get());
        assertEquals("", bloodPressureMeasurementPulseRateReference.get());
        assertFalse(isBloodPressureMeasurementUserIdSupportedReference.get());
        assertEquals("", bloodPressureMeasurementUserIdReference.get());
        assertFalse(isBloodPressureMeasurementMeasurementStatusSupportedReference.get());
        assertEquals("", bloodPressureMeasurementMeasurementStatusReference.get());

        assertEquals("", intermediateCuffPressureFlagsReference.get());
        assertEquals("", intermediateCuffPressureCurrentCuffPressureReference.get());
        assertFalse(isIntermediateCuffPressureTimeStampSupportedReference.get());
        assertEquals("", intermediateCuffPressureTimeStampReference.get());
        assertFalse(intermediateCuffPressurePulseRateSupportedReference.get());
        assertEquals("", intermediateCuffPressurePulseRateReference.get());
        assertFalse(isIntermediateCuffPressureUserIdSupportedReference.get());
        assertEquals("", intermediateCuffPressureUserIdReference.get());
        assertFalse(isIntermediateCuffPressureMeasurementStatusSupportedReference.get());
        assertEquals("", intermediateCuffPressureMeasurementStatusReference.get());

        assertEquals("", bloodPressureFeatureReference.get());
    }

    @Test
    public void test_observeSetup_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasBloodPressureFeatureDataJsonReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureMeasurementFlagsReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementSystolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementDiastolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeanArterialPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> bloodPressureMeasurementPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementPulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> intermediateCuffPressureFlagsReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> intermediateCuffPressurePulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressurePulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureFeatureReference = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJsonReference::set);
        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureSupportedReference::set);
        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJsonReference::set);
        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJsonReference::set);
        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlagsReference::set);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolicReference::set);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolicReference::set);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressureReference::set);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStampReference::set);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateReference::set);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementUserIdSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserIdReference::set);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatusReference::set);

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlagsReference::set);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressureReference::set);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStampReference::set);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateSupportedReference::set);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateReference::set);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserIdReference::set);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatusReference::set);

        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), bloodPressureFeatureReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = BLOOD_PRESSURE_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
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
        serviceData.characteristicDataList.add(bloodPressureMeasurementCharacteristicData);

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        serviceData.characteristicDataList.add(intermediateCuffPressureCharacteristicData);

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData();
        bloodPressureFeatureCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        bloodPressureFeatureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        bloodPressureFeatureCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        boolean isBodyMovementDetectionSupported = false;
        boolean isCuffFitDetectionSupportSupported = false;
        boolean hasIrregularPulseDetection = false;
        boolean hasPulseRateRangeDetection = false;
        boolean isMeasurementPositionDetectionSupported = false;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        serviceData.characteristicDataList.add(bloodPressureFeatureCharacteristicData);

        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(hasBloodPressureMeasurementDataJsonReference.get());
        assertTrue(isIntermediateCuffPressureSupportedReference.get());
        assertTrue(hasIntermediateCuffPressureDataJsonReference.get());
        assertTrue(hasBloodPressureFeatureDataJsonReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementFlags, 2), bloodPressureMeasurementFlagsReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueSystolicMmhg.getSfloat(), Double.parseDouble(bloodPressureMeasurementSystolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueDiastolicMmhg.getSfloat(), Double.parseDouble(bloodPressureMeasurementDiastolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg.getSfloat(), Double.parseDouble(bloodPressureMeanArterialPressureReference.get()));
        assertFalse(isBloodPressureMeasurementTimeStampSupportedReference.get());
        assertEquals("", bloodPressureMeasurementTimeStampReference.get());
        assertFalse(bloodPressureMeasurementPulseRateSupportedReference.get());
        assertEquals("", bloodPressureMeasurementPulseRateReference.get());
        assertFalse(isBloodPressureMeasurementUserIdSupportedReference.get());
        assertEquals("", bloodPressureMeasurementUserIdReference.get());
        assertFalse(isBloodPressureMeasurementMeasurementStatusSupportedReference.get());
        assertEquals("", bloodPressureMeasurementMeasurementStatusReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureFlags, 2), intermediateCuffPressureFlagsReference.get());
        assertEquals(intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg.getSfloat(), Double.parseDouble(intermediateCuffPressureCurrentCuffPressureReference.get()));
        assertFalse(isIntermediateCuffPressureTimeStampSupportedReference.get());
        assertEquals("", intermediateCuffPressureTimeStampReference.get());
        assertFalse(intermediateCuffPressurePulseRateSupportedReference.get());
        assertEquals("", intermediateCuffPressurePulseRateReference.get());
        assertFalse(isIntermediateCuffPressureUserIdSupportedReference.get());
        assertEquals("", intermediateCuffPressureUserIdReference.get());
        assertFalse(isIntermediateCuffPressureMeasurementStatusSupportedReference.get());
        assertEquals("", intermediateCuffPressureMeasurementStatusReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureFeature.getBytes(), 0), 4), bloodPressureFeatureReference.get());
    }

    @Test
    public void test_observeSetup_3_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureSupportedReference = new AtomicReference<>();
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJsonReference = new AtomicReference<>();
        AtomicReference<Boolean> hasBloodPressureFeatureDataJsonReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureMeasurementFlagsReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementSystolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementDiastolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeanArterialPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> bloodPressureMeasurementPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementPulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> intermediateCuffPressureFlagsReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> intermediateCuffPressurePulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressurePulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureMeasurementStatusReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureFeatureReference = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJsonReference::set);
        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureSupportedReference::set);
        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJsonReference::set);
        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJsonReference::set);
        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlagsReference::set);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolicReference::set);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolicReference::set);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressureReference::set);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStampReference::set);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateReference::set);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementUserIdSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserIdReference::set);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatusReference::set);

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlagsReference::set);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressureReference::set);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStampReference::set);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateSupportedReference::set);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateReference::set);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserIdReference::set);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatusReference::set);

        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), bloodPressureFeatureReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = BLOOD_PRESSURE_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

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
        int bloodPressureMeasurementMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
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
        serviceData.characteristicDataList.add(bloodPressureMeasurementCharacteristicData);

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_BLOOD_PRESSURE_UNITS_KPA
                | BloodPressureMeasurementUtils.FLAG_TIME_STAMP_PRESENT
                | BloodPressureMeasurementUtils.FLAG_PULSE_RATE_PRESENT
                | BloodPressureMeasurementUtils.FLAG_USER_ID_PRESENT
                | BloodPressureMeasurementUtils.FLAG_MEASUREMENT_STATUS_PRESENT;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 5555;
        int intermediateCuffPressureMonth = 6;
        int intermediateCuffPressureDay = 7;
        int intermediateCuffPressureHours = 8;
        int intermediateCuffPressureMinutes = 9;
        int intermediateCuffPressureSeconds = 10;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(11);
        int intermediateCuffPressureUserId = 12;
        int intermediateCuffPressureMeasurementStatusFlags = BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                | BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[]{(byte) bloodPressureMeasurementMeasurementStatusFlags,
                (byte) (bloodPressureMeasurementMeasurementStatusFlags >> 8)};
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        serviceData.characteristicDataList.add(intermediateCuffPressureCharacteristicData);

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData();
        bloodPressureFeatureCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        bloodPressureFeatureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        bloodPressureFeatureCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        boolean isBodyMovementDetectionSupported = true;
        boolean isCuffFitDetectionSupportSupported = true;
        boolean hasIrregularPulseDetection = true;
        boolean hasPulseRateRangeDetection = true;
        boolean isMeasurementPositionDetectionSupported = true;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        serviceData.characteristicDataList.add(bloodPressureFeatureCharacteristicData);

        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        assertTrue(hasBloodPressureMeasurementDataJsonReference.get());
        assertTrue(isIntermediateCuffPressureSupportedReference.get());
        assertTrue(hasIntermediateCuffPressureDataJsonReference.get());
        assertTrue(hasBloodPressureFeatureDataJsonReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementFlags, 2), bloodPressureMeasurementFlagsReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueSystolicKpa.getSfloat(), Double.parseDouble(bloodPressureMeasurementSystolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueDiastolicKpa.getSfloat(), Double.parseDouble(bloodPressureMeasurementDiastolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueMeanArterialPressureKpa.getSfloat(), Double.parseDouble(bloodPressureMeanArterialPressureReference.get()));
        assertTrue(isBloodPressureMeasurementTimeStampSupportedReference.get());
        assertEquals(mFakeDeviceSettingRepository.getDateTimeString(bloodPressureMeasurementYear
                , bloodPressureMeasurementMonth
                , bloodPressureMeasurementDay
                , bloodPressureMeasurementHours
                , bloodPressureMeasurementMinutes
                , bloodPressureMeasurementSeconds)
                , bloodPressureMeasurementTimeStampReference.get());
        assertTrue(bloodPressureMeasurementPulseRateSupportedReference.get());
        assertEquals(bloodPressureMeasurementPulseRate.getSfloat(), Double.parseDouble(bloodPressureMeasurementPulseRateReference.get()));
        assertTrue(isBloodPressureMeasurementUserIdSupportedReference.get());
        assertEquals(bloodPressureMeasurementUserId, Integer.parseInt(bloodPressureMeasurementUserIdReference.get()));
        assertTrue(isBloodPressureMeasurementMeasurementStatusSupportedReference.get());
        assertEquals(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementMeasurementStatusFlags, 4), bloodPressureMeasurementMeasurementStatusReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureFlags, 2), intermediateCuffPressureFlagsReference.get());
        assertEquals(intermediateCuffPressureCompoundValueCurrentCuffPressureKpa.getSfloat(), Double.parseDouble(intermediateCuffPressureCurrentCuffPressureReference.get()));
        assertTrue(isIntermediateCuffPressureTimeStampSupportedReference.get());
        assertEquals(mFakeDeviceSettingRepository.getDateTimeString(intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds)
                , intermediateCuffPressureTimeStampReference.get());
        assertTrue(intermediateCuffPressurePulseRateSupportedReference.get());
        assertEquals(intermediateCuffPressurePulseRate.getSfloat(), Double.parseDouble(intermediateCuffPressurePulseRateReference.get()));
        assertTrue(isIntermediateCuffPressureUserIdSupportedReference.get());
        assertEquals(intermediateCuffPressureUserId, Integer.parseInt(intermediateCuffPressureUserIdReference.get()));
        assertTrue(isIntermediateCuffPressureMeasurementStatusSupportedReference.get());
        assertEquals(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureMeasurementStatusFlags, 4), intermediateCuffPressureMeasurementStatusReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(BLEUtils.createUInt16(bloodPressureFeature.getBytes(), 0), 4), bloodPressureFeatureReference.get());
    }

    @Test
    public void test_observeSave_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Already saved", throwableReference.get().getMessage());
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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
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
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(bloodPressureMeasurementCharacteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
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

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData();
        bloodPressureFeatureCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        bloodPressureFeatureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        bloodPressureFeatureCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        boolean isBodyMovementDetectionSupported = true;
        boolean isCuffFitDetectionSupportSupported = true;
        boolean hasIrregularPulseDetection = true;
        boolean hasPulseRateRangeDetection = true;
        boolean isMeasurementPositionDetectionSupported = true;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        mViewModel.setBloodPressureFeatureDataJson(mGson.toJson(bloodPressureFeatureCharacteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
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

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureDataJson(mGson.toJson(intermediateCuffPressureCharacteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
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
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(bloodPressureMeasurementCharacteristicData));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureDataJson(mGson.toJson(intermediateCuffPressureCharacteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
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

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData();
        bloodPressureFeatureCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        bloodPressureFeatureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        bloodPressureFeatureCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        boolean isBodyMovementDetectionSupported = true;
        boolean isCuffFitDetectionSupportSupported = true;
        boolean hasIrregularPulseDetection = true;
        boolean hasPulseRateRangeDetection = true;
        boolean isMeasurementPositionDetectionSupported = true;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        mViewModel.setBloodPressureFeatureDataJson(mGson.toJson(bloodPressureFeatureCharacteristicData));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureDataJson(mGson.toJson(intermediateCuffPressureCharacteristicData));

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("No data", throwableReference.get().getMessage());
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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
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
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(bloodPressureMeasurementCharacteristicData));

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData();
        bloodPressureFeatureCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        bloodPressureFeatureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        bloodPressureFeatureCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        boolean isBodyMovementDetectionSupported = true;
        boolean isCuffFitDetectionSupportSupported = true;
        boolean hasIrregularPulseDetection = true;
        boolean hasPulseRateRangeDetection = true;
        boolean isMeasurementPositionDetectionSupported = true;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        mViewModel.setBloodPressureFeatureDataJson(mGson.toJson(bloodPressureFeatureCharacteristicData));

        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> serviceDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_SERVICE.toString()), ServiceData.class))
                , throwable -> {
                });

        ServiceData resultServiceData = serviceDataAtomicReference.get();
        assertNotNull(resultServiceData);

        Optional<CharacteristicData> bloodPressureMeasurementOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC))
                .findAny();

        assertTrue(bloodPressureMeasurementOptional.isPresent());
        CharacteristicData resultBloodPressureMeasurementCharacteristicData = bloodPressureMeasurementOptional.get();
        assertEquals(bloodPressureMeasurementCharacteristicData, resultBloodPressureMeasurementCharacteristicData);

        Optional<CharacteristicData> bloodPressureFeatureOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC))
                .findAny();
        assertTrue(bloodPressureFeatureOptional.isPresent());
        CharacteristicData resultBloodPressureFeatureCharacteristicData = bloodPressureFeatureOptional.get();
        assertEquals(bloodPressureFeatureCharacteristicData, resultBloodPressureFeatureCharacteristicData);
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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
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
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(bloodPressureMeasurementCharacteristicData));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureDataJson(mGson.toJson(intermediateCuffPressureCharacteristicData));

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData();
        bloodPressureFeatureCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        bloodPressureFeatureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        bloodPressureFeatureCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        boolean isBodyMovementDetectionSupported = true;
        boolean isCuffFitDetectionSupportSupported = true;
        boolean hasIrregularPulseDetection = true;
        boolean hasPulseRateRangeDetection = true;
        boolean isMeasurementPositionDetectionSupported = true;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        mViewModel.setBloodPressureFeatureDataJson(mGson.toJson(bloodPressureFeatureCharacteristicData));

        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> serviceDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_SERVICE.toString()), ServiceData.class))
                , throwable -> {
                });

        ServiceData resultServiceData = serviceDataAtomicReference.get();
        assertNotNull(resultServiceData);

        Optional<CharacteristicData> bloodPressureMeasurementOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC))
                .findAny();

        assertTrue(bloodPressureMeasurementOptional.isPresent());
        CharacteristicData resultBloodPressureMeasurementCharacteristicData = bloodPressureMeasurementOptional.get();
        assertEquals(bloodPressureMeasurementCharacteristicData, resultBloodPressureMeasurementCharacteristicData);

        Optional<CharacteristicData> intermediateCuffPressureOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC))
                .findAny();

        assertFalse(intermediateCuffPressureOptional.isPresent());

        Optional<CharacteristicData> bloodPressureFeatureOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC))
                .findAny();
        assertTrue(bloodPressureFeatureOptional.isPresent());
        CharacteristicData resultBloodPressureFeatureCharacteristicData = bloodPressureFeatureOptional.get();
        assertEquals(bloodPressureFeatureCharacteristicData, resultBloodPressureFeatureCharacteristicData);
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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
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
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(bloodPressureMeasurementCharacteristicData));

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        mViewModel.setIntermediateCuffPressureDataJson(mGson.toJson(intermediateCuffPressureCharacteristicData));

        CharacteristicData bloodPressureFeatureCharacteristicData = new CharacteristicData();
        bloodPressureFeatureCharacteristicData.uuid = BLOOD_PRESSURE_FEATURE_CHARACTERISTIC;
        bloodPressureFeatureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_READ;
        bloodPressureFeatureCharacteristicData.permission = BluetoothGattCharacteristic.PERMISSION_READ;
        boolean isBodyMovementDetectionSupported = true;
        boolean isCuffFitDetectionSupportSupported = true;
        boolean hasIrregularPulseDetection = true;
        boolean hasPulseRateRangeDetection = true;
        boolean isMeasurementPositionDetectionSupported = true;
        boolean isMultipleBondSupported = true;
        BloodPressureFeature bloodPressureFeature = new BloodPressureFeature(isBodyMovementDetectionSupported
                , isCuffFitDetectionSupportSupported
                , hasIrregularPulseDetection
                , hasPulseRateRangeDetection
                , isMeasurementPositionDetectionSupported
                , isMultipleBondSupported
                , false
                , false
                , false);
        bloodPressureFeatureCharacteristicData.data = bloodPressureFeature.getBytes();
        mViewModel.setBloodPressureFeatureDataJson(mGson.toJson(bloodPressureFeatureCharacteristicData));

        mViewModel.updateIsIntermediateCuffPressureSupported(true);
        AtomicReference<ServiceData> serviceDataAtomicReference = new AtomicReference<>();
        mViewModel.observeSave(resultIntent
                        -> serviceDataAtomicReference.set(mGson.fromJson(resultIntent.getStringExtra(BLOOD_PRESSURE_SERVICE.toString()), ServiceData.class))
                , throwable -> {
                });

        ServiceData resultServiceData = serviceDataAtomicReference.get();
        assertNotNull(resultServiceData);

        Optional<CharacteristicData> bloodPressureMeasurementOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC))
                .findAny();

        assertTrue(bloodPressureMeasurementOptional.isPresent());
        CharacteristicData resultBloodPressureMeasurementCharacteristicData = bloodPressureMeasurementOptional.get();
        assertEquals(bloodPressureMeasurementCharacteristicData, resultBloodPressureMeasurementCharacteristicData);

        Optional<CharacteristicData> intermediateCuffPressureOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC))
                .findAny();

        assertTrue(intermediateCuffPressureOptional.isPresent());
        CharacteristicData resultIntermediateCuffPressureCharacteristicData = intermediateCuffPressureOptional.get();
        assertEquals(intermediateCuffPressureCharacteristicData, resultIntermediateCuffPressureCharacteristicData);

        Optional<CharacteristicData> bloodPressureFeatureOptional = resultServiceData.characteristicDataList
                .stream()
                .filter(filterTarget -> filterTarget.uuid.equals(BLOOD_PRESSURE_FEATURE_CHARACTERISTIC))
                .findAny();
        assertTrue(bloodPressureFeatureOptional.isPresent());
        CharacteristicData resultBloodPressureFeatureCharacteristicData = bloodPressureFeatureOptional.get();
        assertEquals(bloodPressureFeatureCharacteristicData, resultBloodPressureFeatureCharacteristicData);
    }

    @Test
    public void test_observeHasBloodPressureMeasurementDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJson = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJson::set);

        TestCase.assertNull(hasBloodPressureMeasurementDataJson.get());
    }

    @Test
    public void test_observeHasBloodPressureMeasurementDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasBloodPressureMeasurementDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", original);
        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJson::set);

        assertTrue(hasBloodPressureMeasurementDataJson.get());
    }

    @Test
    public void test_observeHasBloodPressureMeasurementDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasBloodPressureMeasurementDataJson = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJson::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", original);

        assertTrue(hasBloodPressureMeasurementDataJson.get());
    }

    @Test
    public void test_observeHasBloodPressureMeasurementDataJson_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasBloodPressureMeasurementDataJson = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasBloodPressureMeasurementDataJson.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", original);

        assertTrue(hasBloodPressureMeasurementDataJson.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasBloodPressureMeasurementDataJson_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasBloodPressureMeasurementDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", original);
        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJson::set);

        assertFalse(hasBloodPressureMeasurementDataJson.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isIntermediateCuffPressureSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureSupported::set);

        TestCase.assertNull(isIntermediateCuffPressureSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isIntermediateCuffPressureSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", original);
        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureSupported::set);

        assertEquals(original, isIntermediateCuffPressureSupported.get().booleanValue());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> isIntermediateCuffPressureSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureSupported::set);
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", original);

        assertEquals(original, isIntermediateCuffPressureSupported.get().booleanValue());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isIntermediateCuffPressureSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isIntermediateCuffPressureSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", original);
        mSavedStateHandle.set("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED", original);

        assertEquals(original, isIntermediateCuffPressureSupported.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasIntermediateCuffPressureDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasIntermediateCuffPressureDataJson = new AtomicReference<>();

        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJson::set);

        TestCase.assertNull(hasIntermediateCuffPressureDataJson.get());
    }

    @Test
    public void test_observeHasIntermediateCuffPressureDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", original);
        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJson::set);

        assertTrue(hasIntermediateCuffPressureDataJson.get());
    }

    @Test
    public void test_observeHasIntermediateCuffPressureDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJson = new AtomicReference<>();

        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJson::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", original);

        assertTrue(hasIntermediateCuffPressureDataJson.get());
    }

    @Test
    public void test_observeHasIntermediateCuffPressureDataJson_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJson = new AtomicReference<>();

        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasIntermediateCuffPressureDataJson.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", original);

        assertTrue(hasIntermediateCuffPressureDataJson.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasIntermediateCuffPressureDataJson_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasIntermediateCuffPressureDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", original);
        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJson::set);

        assertFalse(hasIntermediateCuffPressureDataJson.get());
    }

    @Test
    public void test_observeHasBloodPressureFeatureDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBloodPressureFeatureDataJson = new AtomicReference<>();

        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJson::set);

        TestCase.assertNull(hasBloodPressureFeatureDataJson.get());
    }

    @Test
    public void test_observeHasBloodPressureFeatureDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasBloodPressureFeatureDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", original);
        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJson::set);

        assertTrue(hasBloodPressureFeatureDataJson.get());
    }

    @Test
    public void test_observeHasBloodPressureFeatureDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> hasBloodPressureFeatureDataJson = new AtomicReference<>();

        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJson::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", original);

        assertTrue(hasBloodPressureFeatureDataJson.get());
    }

    @Test
    public void test_observeHasBloodPressureFeatureDataJson_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> hasBloodPressureFeatureDataJson = new AtomicReference<>();

        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            hasBloodPressureFeatureDataJson.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", original);

        assertTrue(hasBloodPressureFeatureDataJson.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeHasBloodPressureFeatureDataJson_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = null;
        AtomicReference<Boolean> hasBloodPressureFeatureDataJson = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE_DATA_JSON", original);
        mViewModel.observeHasBloodPressureFeatureDataJson(new TestLifeCycleOwner(), hasBloodPressureFeatureDataJson::set);

        assertFalse(hasBloodPressureFeatureDataJson.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementFlags_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeasurementFlags = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlags::set);

        TestCase.assertNull(bloodPressureMeasurementFlags.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementFlags_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementFlags = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS", original);
        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlags::set);

        assertEquals(original, bloodPressureMeasurementFlags.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementFlags_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementFlags = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlags::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS", original);

        assertEquals(original, bloodPressureMeasurementFlags.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementFlags_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeasurementFlags = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeasurementFlags.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_FLAGS", original);

        assertEquals(original, bloodPressureMeasurementFlags.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementSystolic_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeasurementSystolic = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolic::set);

        TestCase.assertNull(bloodPressureMeasurementSystolic.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementSystolic_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementSystolic = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC", original);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolic::set);

        assertEquals(original, bloodPressureMeasurementSystolic.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementSystolic_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementSystolic = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolic::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC", original);

        assertEquals(original, bloodPressureMeasurementSystolic.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementSystolic_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeasurementSystolic = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeasurementSystolic.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_SYSTOLIC", original);

        assertEquals(original, bloodPressureMeasurementSystolic.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementDiastolic_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeasurementDiastolic = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolic::set);

        TestCase.assertNull(bloodPressureMeasurementDiastolic.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementDiastolic_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementDiastolic = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC", original);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolic::set);

        assertEquals(original, bloodPressureMeasurementDiastolic.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementDiastolic_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementDiastolic = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolic::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC", original);

        assertEquals(original, bloodPressureMeasurementDiastolic.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementDiastolic_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeasurementDiastolic = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeasurementDiastolic.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DIASTOLIC", original);

        assertEquals(original, bloodPressureMeasurementDiastolic.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureMeanArterialPressure_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeanArterialPressure = new AtomicReference<>();

        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressure::set);

        TestCase.assertNull(bloodPressureMeanArterialPressure.get());
    }

    @Test
    public void test_observeBloodPressureMeanArterialPressure_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeanArterialPressure = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE", original);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressure::set);

        assertEquals(original, bloodPressureMeanArterialPressure.get());
    }

    @Test
    public void test_observeBloodPressureMeanArterialPressure_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeanArterialPressure = new AtomicReference<>();

        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressure::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE", original);

        assertEquals(original, bloodPressureMeanArterialPressure.get());
    }

    @Test
    public void test_observeBloodPressureMeanArterialPressure_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeanArterialPressure = new AtomicReference<>();

        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeanArterialPressure.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEAN_ARTERIAL_PRESSURE", original);

        assertEquals(original, bloodPressureMeanArterialPressure.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementTimeStampSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupported::set);

        assertNull(isBloodPressureMeasurementTimeStampSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementTimeStampSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupported::set);

        assertTrue(isBloodPressureMeasurementTimeStampSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementTimeStampSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupported::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);

        assertTrue(isBloodPressureMeasurementTimeStampSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementTimeStampSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isBloodPressureMeasurementTimeStampSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);

        assertTrue(isBloodPressureMeasurementTimeStampSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementTimeStamp_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeasurementTimeStamp = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStamp::set);

        TestCase.assertNull(bloodPressureMeasurementTimeStamp.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementTimeStamp_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementTimeStamp = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStamp::set);

        assertEquals(original, bloodPressureMeasurementTimeStamp.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementTimeStamp_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementTimeStamp = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStamp::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);

        assertEquals(original, bloodPressureMeasurementTimeStamp.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementTimeStamp_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeasurementTimeStamp = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeasurementTimeStamp.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_TIME_STAMP", original);

        assertEquals(original, bloodPressureMeasurementTimeStamp.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementPulseRateSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementPulseRateSupported::set);

        assertNull(isBloodPressureMeasurementPulseRateSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementPulseRateSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementPulseRateSupported::set);

        assertTrue(isBloodPressureMeasurementPulseRateSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementPulseRateSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementPulseRateSupported::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);

        assertTrue(isBloodPressureMeasurementPulseRateSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementPulseRateSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isBloodPressureMeasurementPulseRateSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);

        assertTrue(isBloodPressureMeasurementPulseRateSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementPulseRate_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeasurementPulseRate = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRate::set);

        TestCase.assertNull(bloodPressureMeasurementPulseRate.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementPulseRate_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementPulseRate = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRate::set);

        assertEquals(original, bloodPressureMeasurementPulseRate.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementPulseRate_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementPulseRate = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRate::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);

        assertEquals(original, bloodPressureMeasurementPulseRate.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementPulseRate_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeasurementPulseRate = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeasurementPulseRate.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_PULSE_RATE", original);

        assertEquals(original, bloodPressureMeasurementPulseRate.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementUserIdSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementPulseRateSupported::set);

        assertNull(isBloodPressureMeasurementPulseRateSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementUserIdSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementPulseRateSupported::set);

        assertTrue(isBloodPressureMeasurementPulseRateSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementUserIdSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementPulseRateSupported::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);

        assertTrue(isBloodPressureMeasurementPulseRateSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementUserIdSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isBloodPressureMeasurementPulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isBloodPressureMeasurementPulseRateSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);

        assertTrue(isBloodPressureMeasurementPulseRateSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementUserId_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeasurementUserId = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserId::set);

        TestCase.assertNull(bloodPressureMeasurementUserId.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementUserId_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementUserId = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserId::set);

        assertEquals(original, bloodPressureMeasurementUserId.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementUserId_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementUserId = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserId::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);

        assertEquals(original, bloodPressureMeasurementUserId.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementUserId_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeasurementUserId = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeasurementUserId.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_USER_ID", original);

        assertEquals(original, bloodPressureMeasurementUserId.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementMeasurementStatusSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupported::set);

        assertNull(isBloodPressureMeasurementMeasurementStatusSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementMeasurementStatusSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupported::set);

        assertTrue(isBloodPressureMeasurementMeasurementStatusSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementMeasurementStatusSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupported::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);

        assertTrue(isBloodPressureMeasurementMeasurementStatusSupported.get());
    }

    @Test
    public void test_observeIsBloodPressureMeasurementMeasurementStatusSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupported = new AtomicReference<>();

        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isBloodPressureMeasurementMeasurementStatusSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);

        assertTrue(isBloodPressureMeasurementMeasurementStatusSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementMeasurementStatus_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureMeasurementMeasurementStatus = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatus::set);

        TestCase.assertNull(bloodPressureMeasurementMeasurementStatus.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementMeasurementStatus_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementMeasurementStatus = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatus::set);

        assertEquals(original, bloodPressureMeasurementMeasurementStatus.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementMeasurementStatus_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureMeasurementMeasurementStatus = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatus::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);

        assertEquals(original, bloodPressureMeasurementMeasurementStatus.get());
    }

    @Test
    public void test_observeBloodPressureMeasurementMeasurementStatus_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureMeasurementMeasurementStatus = new AtomicReference<>();

        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureMeasurementMeasurementStatus.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_MEASUREMENT_STATUS", original);

        assertEquals(original, bloodPressureMeasurementMeasurementStatus.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureFlags_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> intermediateCuffPressureFlags = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlags::set);

        TestCase.assertNull(intermediateCuffPressureFlags.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureFlags_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureFlags = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS", original);
        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlags::set);

        assertEquals(original, intermediateCuffPressureFlags.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureFlags_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureFlags = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlags::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS", original);

        assertEquals(original, intermediateCuffPressureFlags.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureFlags_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> intermediateCuffPressureFlags = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            intermediateCuffPressureFlags.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_FLAGS", original);

        assertEquals(original, intermediateCuffPressureFlags.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureCurrentCuffPressure_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> intermediateCuffPressureCurrentCuffPressure = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressure::set);

        TestCase.assertNull(intermediateCuffPressureCurrentCuffPressure.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureCurrentCuffPressure_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressure = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE", original);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressure::set);

        assertEquals(original, intermediateCuffPressureCurrentCuffPressure.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureCurrentCuffPressure_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressure = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressure::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE", original);

        assertEquals(original, intermediateCuffPressureCurrentCuffPressure.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureCurrentCuffPressure_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressure = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            intermediateCuffPressureCurrentCuffPressure.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_CURRENT_CUFF_PRESSURE", original);

        assertEquals(original, intermediateCuffPressureCurrentCuffPressure.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureTimeStampSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupported::set);

        assertNull(isIntermediateCuffPressureTimeStampSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureTimeStampSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupported::set);

        assertTrue(isIntermediateCuffPressureTimeStampSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureTimeStampSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupported::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);

        assertTrue(isIntermediateCuffPressureTimeStampSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureTimeStampSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isIntermediateCuffPressureTimeStampSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);

        assertTrue(isIntermediateCuffPressureTimeStampSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureTimeStamp_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> intermediateCuffPressureTimeStamp = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStamp::set);

        TestCase.assertNull(intermediateCuffPressureTimeStamp.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureTimeStamp_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureTimeStamp = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStamp::set);

        assertEquals(original, intermediateCuffPressureTimeStamp.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureTimeStamp_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureTimeStamp = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStamp::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);

        assertEquals(original, intermediateCuffPressureTimeStamp.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureTimeStamp_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> intermediateCuffPressureTimeStamp = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            intermediateCuffPressureTimeStamp.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_TIME_STAMP", original);

        assertEquals(original, intermediateCuffPressureTimeStamp.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressurePulseRateSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isIntermediateCuffPressurePulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), isIntermediateCuffPressurePulseRateSupported::set);

        assertNull(isIntermediateCuffPressurePulseRateSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressurePulseRateSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressurePulseRateSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), isIntermediateCuffPressurePulseRateSupported::set);

        assertTrue(isIntermediateCuffPressurePulseRateSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressurePulseRateSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressurePulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), isIntermediateCuffPressurePulseRateSupported::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);

        assertTrue(isIntermediateCuffPressurePulseRateSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressurePulseRateSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isIntermediateCuffPressurePulseRateSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isIntermediateCuffPressurePulseRateSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);

        assertTrue(isIntermediateCuffPressurePulseRateSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIntermediateCuffPressurePulseRate_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> intermediateCuffPressurePulseRate = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRate::set);

        TestCase.assertNull(intermediateCuffPressurePulseRate.get());
    }

    @Test
    public void test_observeIntermediateCuffPressurePulseRate_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressurePulseRate = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRate::set);

        assertEquals(original, intermediateCuffPressurePulseRate.get());
    }

    @Test
    public void test_observeIntermediateCuffPressurePulseRate_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressurePulseRate = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRate::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);

        assertEquals(original, intermediateCuffPressurePulseRate.get());
    }

    @Test
    public void test_observeIntermediateCuffPressurePulseRate_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> intermediateCuffPressurePulseRate = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            intermediateCuffPressurePulseRate.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_PULSE_RATE", original);

        assertEquals(original, intermediateCuffPressurePulseRate.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureUserIdSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupported::set);

        assertNull(isIntermediateCuffPressureUserIdSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureUserIdSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupported::set);

        assertTrue(isIntermediateCuffPressureUserIdSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureUserIdSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupported::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);

        assertTrue(isIntermediateCuffPressureUserIdSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureUserIdSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isIntermediateCuffPressureUserIdSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);

        assertTrue(isIntermediateCuffPressureUserIdSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureUserId_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> intermediateCuffPressureUserId = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserId::set);

        TestCase.assertNull(intermediateCuffPressureUserId.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureUserId_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureUserId = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserId::set);

        assertEquals(original, intermediateCuffPressureUserId.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureUserId_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureUserId = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserId::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);

        assertEquals(original, intermediateCuffPressureUserId.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureUserId_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> intermediateCuffPressureUserId = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            intermediateCuffPressureUserId.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_USER_ID", original);

        assertEquals(original, intermediateCuffPressureUserId.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureMeasurementStatusSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupported::set);

        assertNull(isIntermediateCuffPressureMeasurementStatusSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureMeasurementStatusSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupported = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupported::set);

        assertTrue(isIntermediateCuffPressureMeasurementStatusSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureMeasurementStatusSupported_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupported::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);

        assertTrue(isIntermediateCuffPressureMeasurementStatusSupported.get());
    }

    @Test
    public void test_observeIsIntermediateCuffPressureMeasurementStatusSupported_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupported = new AtomicReference<>();

        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            isIntermediateCuffPressureMeasurementStatusSupported.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);

        assertTrue(isIntermediateCuffPressureMeasurementStatusSupported.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureMeasurementStatus_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> intermediateCuffPressureMeasurementStatus = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatus::set);

        TestCase.assertNull(intermediateCuffPressureMeasurementStatus.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureMeasurementStatus_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureMeasurementStatus = new AtomicReference<>();

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatus::set);

        assertEquals(original, intermediateCuffPressureMeasurementStatus.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureMeasurementStatus_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> intermediateCuffPressureMeasurementStatus = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatus::set);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);

        assertEquals(original, intermediateCuffPressureMeasurementStatus.get());
    }

    @Test
    public void test_observeIntermediateCuffPressureMeasurementStatus_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> intermediateCuffPressureMeasurementStatus = new AtomicReference<>();

        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            intermediateCuffPressureMeasurementStatus.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);
        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_MEASUREMENT_STATUS", original);

        assertEquals(original, intermediateCuffPressureMeasurementStatus.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeBloodPressureFeature_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> bloodPressureFeature = new AtomicReference<>();

        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), bloodPressureFeature::set);

        TestCase.assertNull(bloodPressureFeature.get());
    }

    @Test
    public void test_observeBloodPressureFeature_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureFeature = new AtomicReference<>();

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE", original);
        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), bloodPressureFeature::set);

        assertEquals(original, bloodPressureFeature.get());
    }

    @Test
    public void test_observeBloodPressureFeature_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> bloodPressureFeature = new AtomicReference<>();

        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), bloodPressureFeature::set);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE", original);

        assertEquals(original, bloodPressureFeature.get());
    }

    @Test
    public void test_observeBloodPressureFeature_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> bloodPressureFeature = new AtomicReference<>();

        mViewModel.observeBloodPressureFeature(new TestLifeCycleOwner(), aBoolean -> {
            count.incrementAndGet();
            bloodPressureFeature.set(aBoolean);
        });
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE", original);
        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_FEATURE", original);

        assertEquals(original, bloodPressureFeature.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_updateIsIntermediateCuffPressureSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean after = true;

        TestCase.assertNull(mSavedStateHandle.get("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED"));
        mViewModel.updateIsIntermediateCuffPressureSupported(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED").booleanValue());
    }

    @Test
    public void test_updateIsIntermediateCuffPressureSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean before = false;
        boolean after = true;

        mViewModel.updateIsIntermediateCuffPressureSupported(before);
        assertEquals(before, mSavedStateHandle.<Boolean>get("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED").booleanValue());

        mViewModel.updateIsIntermediateCuffPressureSupported(after);

        assertEquals(after, mSavedStateHandle.<Boolean>get("KEY_IS_INTERMEDIATE_CUFF_PRESSURE_SUPPORTED").booleanValue());
    }

    @Test
    public void test_getBloodPressureMeasurementDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        TestCase.assertNull(mViewModel.getBloodPressureMeasurementDataJson());
    }

    @Test
    public void test_getBloodPressureMeasurementDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";

        mSavedStateHandle.set("KEY_BLOOD_PRESSURE_MEASUREMENT_DATA_JSON", original);
        assertEquals(original, mViewModel.getBloodPressureMeasurementDataJson());
    }

    @Test
    public void test_setBloodPressureMeasurementDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJsonReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureMeasurementFlagsReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementSystolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementDiastolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeanArterialPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> bloodPressureMeasurementPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementPulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementMeasurementStatusReference = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJsonReference::set);

        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlagsReference::set);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolicReference::set);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolicReference::set);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressureReference::set);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStampReference::set);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateReference::set);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementUserIdSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserIdReference::set);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatusReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
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
        String originalJson = mGson.toJson(bloodPressureMeasurementCharacteristicData);
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(bloodPressureMeasurementCharacteristicData));

        assertEquals(originalJson, mViewModel.getBloodPressureMeasurementDataJson());
        assertTrue(hasBloodPressureMeasurementDataJsonReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementFlags, 2), bloodPressureMeasurementFlagsReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueSystolicMmhg.getSfloat(), Double.parseDouble(bloodPressureMeasurementSystolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueDiastolicMmhg.getSfloat(), Double.parseDouble(bloodPressureMeasurementDiastolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueMeanArterialPressureMmhg.getSfloat(), Double.parseDouble(bloodPressureMeanArterialPressureReference.get()));
        assertFalse(isBloodPressureMeasurementTimeStampSupportedReference.get());
        assertEquals("", bloodPressureMeasurementTimeStampReference.get());
        assertFalse(bloodPressureMeasurementPulseRateSupportedReference.get());
        assertEquals("", bloodPressureMeasurementPulseRateReference.get());
        assertFalse(isBloodPressureMeasurementUserIdSupportedReference.get());
        assertEquals("", bloodPressureMeasurementUserIdReference.get());
        assertFalse(isBloodPressureMeasurementMeasurementStatusSupportedReference.get());
        assertEquals("", bloodPressureMeasurementMeasurementStatusReference.get());
    }

    @Test
    public void test_setBloodPressureMeasurementDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJsonReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureMeasurementFlagsReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementSystolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementDiastolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeanArterialPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> bloodPressureMeasurementPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementPulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementMeasurementStatusReference = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJsonReference::set);

        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlagsReference::set);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolicReference::set);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolicReference::set);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressureReference::set);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStampReference::set);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateReference::set);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementUserIdSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserIdReference::set);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatusReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = BLOOD_PRESSURE_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
        int flags = 0;
        IEEE_11073_20601_SFLOAT systolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT diastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT meanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT systolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT diastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT manArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int year = 7777;
        int month = 8;
        int day = 9;
        int hours = 10;
        int minutes = 11;
        int seconds = 12;
        IEEE_11073_20601_SFLOAT pulseRate = new IEEE_11073_20601_SFLOAT(13);
        int userId = 14;
        byte[] measurementStatus = new byte[2];
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(flags
                , systolicMmhg
                , diastolicMmhg
                , meanArterialPressureMmhg
                , systolicKpa
                , diastolicKpa
                , manArterialPressureKpa
                , year
                , month
                , day
                , hours
                , minutes
                , seconds
                , pulseRate
                , userId
                , measurementStatus);
        characteristicData.data = bloodPressureMeasurement.getBytes();
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        CharacteristicData bloodPressureMeasurementCharacteristicData = new CharacteristicData();
        bloodPressureMeasurementCharacteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
        bloodPressureMeasurementCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
        int bloodPressureMeasurementFlags = BloodPressureMeasurementUtils.FLAG_BLOOD_PRESSURE_UNITS_KPA;
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
        byte[] bloodPressureMeasurementMeasurementStatus = new byte[2];
        bloodPressureMeasurement = new BloodPressureMeasurement(bloodPressureMeasurementFlags
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
        String originalJson = mGson.toJson(bloodPressureMeasurementCharacteristicData);
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(bloodPressureMeasurementCharacteristicData));

        assertEquals(originalJson, mViewModel.getBloodPressureMeasurementDataJson());
        assertTrue(hasBloodPressureMeasurementDataJsonReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(bloodPressureMeasurementFlags, 2), bloodPressureMeasurementFlagsReference.get());
        assertEquals(bloodPressureMeasurementCompoundValueSystolicKpa.getSfloat(), Double.parseDouble(bloodPressureMeasurementSystolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueDiastolicKpa.getSfloat(), Double.parseDouble(bloodPressureMeasurementDiastolicReference.get()));
        assertEquals(bloodPressureMeasurementCompoundValueMeanArterialPressureKpa.getSfloat(), Double.parseDouble(bloodPressureMeanArterialPressureReference.get()));
        assertFalse(isBloodPressureMeasurementTimeStampSupportedReference.get());
        assertEquals("", bloodPressureMeasurementTimeStampReference.get());
        assertFalse(bloodPressureMeasurementPulseRateSupportedReference.get());
        assertEquals("", bloodPressureMeasurementPulseRateReference.get());
        assertFalse(isBloodPressureMeasurementUserIdSupportedReference.get());
        assertEquals("", bloodPressureMeasurementUserIdReference.get());
        assertFalse(isBloodPressureMeasurementMeasurementStatusSupportedReference.get());
        assertEquals("", bloodPressureMeasurementMeasurementStatusReference.get());
    }

    @Test
    public void test_setBloodPressureMeasurementDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBloodPressureMeasurementDataJsonReference = new AtomicReference<>();

        AtomicReference<String> bloodPressureMeasurementFlagsReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementSystolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementDiastolicReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeanArterialPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> bloodPressureMeasurementPulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementPulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isBloodPressureMeasurementMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> bloodPressureMeasurementMeasurementStatusReference = new AtomicReference<>();

        mViewModel.observeHasBloodPressureMeasurementDataJson(new TestLifeCycleOwner(), hasBloodPressureMeasurementDataJsonReference::set);

        mViewModel.observeBloodPressureMeasurementFlags(new TestLifeCycleOwner(), bloodPressureMeasurementFlagsReference::set);
        mViewModel.observeBloodPressureMeasurementSystolic(new TestLifeCycleOwner(), bloodPressureMeasurementSystolicReference::set);
        mViewModel.observeBloodPressureMeasurementDiastolic(new TestLifeCycleOwner(), bloodPressureMeasurementDiastolicReference::set);
        mViewModel.observeBloodPressureMeanArterialPressure(new TestLifeCycleOwner(), bloodPressureMeanArterialPressureReference::set);
        mViewModel.observeIsBloodPressureMeasurementTimeStampSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementTimeStampSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementTimeStamp(new TestLifeCycleOwner(), bloodPressureMeasurementTimeStampReference::set);
        mViewModel.observeIsBloodPressureMeasurementPulseRateSupported(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementPulseRate(new TestLifeCycleOwner(), bloodPressureMeasurementPulseRateReference::set);
        mViewModel.observeIsBloodPressureMeasurementUserIdSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementUserIdSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementUserId(new TestLifeCycleOwner(), bloodPressureMeasurementUserIdReference::set);
        mViewModel.observeIsBloodPressureMeasurementMeasurementStatusSupported(new TestLifeCycleOwner(), isBloodPressureMeasurementMeasurementStatusSupportedReference::set);
        mViewModel.observeBloodPressureMeasurementMeasurementStatus(new TestLifeCycleOwner(), bloodPressureMeasurementMeasurementStatusReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = BLOOD_PRESSURE_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = BLOOD_PRESSURE_MEASUREMENT_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_INDICATE;
        int flags = 0;
        IEEE_11073_20601_SFLOAT systolicMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT diastolicMmhg = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT meanArterialPressureMmhg = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT systolicKpa = new IEEE_11073_20601_SFLOAT(4);
        IEEE_11073_20601_SFLOAT diastolicKpa = new IEEE_11073_20601_SFLOAT(5);
        IEEE_11073_20601_SFLOAT manArterialPressureKpa = new IEEE_11073_20601_SFLOAT(6);
        int year = 7777;
        int month = 8;
        int day = 9;
        int hours = 10;
        int minutes = 11;
        int seconds = 12;
        IEEE_11073_20601_SFLOAT pulseRate = new IEEE_11073_20601_SFLOAT(13);
        int userId = 14;
        byte[] measurementStatus = new byte[2];
        BloodPressureMeasurement bloodPressureMeasurement = new BloodPressureMeasurement(flags
                , systolicMmhg
                , diastolicMmhg
                , meanArterialPressureMmhg
                , systolicKpa
                , diastolicKpa
                , manArterialPressureKpa
                , year
                , month
                , day
                , hours
                , minutes
                , seconds
                , pulseRate
                , userId
                , measurementStatus);
        characteristicData.data = bloodPressureMeasurement.getBytes();
        mViewModel.setBloodPressureMeasurementDataJson(mGson.toJson(characteristicData));
        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setBloodPressureMeasurementDataJson(null);

        assertNull(mViewModel.getBloodPressureMeasurementDataJson());
        assertFalse(hasBloodPressureMeasurementDataJsonReference.get());

        assertEquals("", bloodPressureMeasurementFlagsReference.get());
        assertEquals("", bloodPressureMeasurementSystolicReference.get());
        assertEquals("", bloodPressureMeasurementDiastolicReference.get());
        assertEquals("", bloodPressureMeanArterialPressureReference.get());
        assertFalse(isBloodPressureMeasurementTimeStampSupportedReference.get());
        assertEquals("", bloodPressureMeasurementTimeStampReference.get());
        assertFalse(bloodPressureMeasurementPulseRateSupportedReference.get());
        assertEquals("", bloodPressureMeasurementPulseRateReference.get());
        assertFalse(isBloodPressureMeasurementUserIdSupportedReference.get());
        assertEquals("", bloodPressureMeasurementUserIdReference.get());
        assertFalse(isBloodPressureMeasurementMeasurementStatusSupportedReference.get());
        assertEquals("", bloodPressureMeasurementMeasurementStatusReference.get());
    }

    @Test
    public void test_getIntermediateCuffPressureDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        TestCase.assertNull(mViewModel.getIntermediateCuffPressureDataJson());
    }

    @Test
    public void test_getIntermediateCuffPressureDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";

        mSavedStateHandle.set("KEY_INTERMEDIATE_CUFF_PRESSURE_DATA_JSON", original);
        assertEquals(original, mViewModel.getIntermediateCuffPressureDataJson());
    }

    @Test
    public void test_setIntermediateCuffPressureDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasIntermediateCuffPressureDataJsonReference = new AtomicReference<>();

        AtomicReference<String> intermediateCuffPressureFlagsReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> intermediateCuffPressurePulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressurePulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureMeasurementStatusReference = new AtomicReference<>();

        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJsonReference::set);

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlagsReference::set);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressureReference::set);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStampReference::set);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateSupportedReference::set);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateReference::set);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserIdReference::set);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatusReference::set);

        Intent intent = new Intent();
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = 0;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        String originalJson = mGson.toJson(intermediateCuffPressureCharacteristicData);
        mViewModel.setIntermediateCuffPressureDataJson(originalJson);

        assertTrue(hasIntermediateCuffPressureDataJsonReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureFlags, 2), intermediateCuffPressureFlagsReference.get());
        assertEquals(intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg.getSfloat(), Double.parseDouble(intermediateCuffPressureCurrentCuffPressureReference.get()));
        assertFalse(isIntermediateCuffPressureTimeStampSupportedReference.get());
        assertEquals("", intermediateCuffPressureTimeStampReference.get());
        assertFalse(intermediateCuffPressurePulseRateSupportedReference.get());
        assertEquals("", intermediateCuffPressurePulseRateReference.get());
        assertFalse(isIntermediateCuffPressureUserIdSupportedReference.get());
        assertEquals("", intermediateCuffPressureUserIdReference.get());
        assertFalse(isIntermediateCuffPressureMeasurementStatusSupportedReference.get());
        assertEquals("", intermediateCuffPressureMeasurementStatusReference.get());
    }

    @Test
    public void test_setIntermediateCuffPressureDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasIntermediateCuffPressureDataJsonReference = new AtomicReference<>();

        AtomicReference<String> intermediateCuffPressureFlagsReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> intermediateCuffPressurePulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressurePulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureMeasurementStatusReference = new AtomicReference<>();

        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJsonReference::set);

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlagsReference::set);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressureReference::set);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStampReference::set);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateSupportedReference::set);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateReference::set);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserIdReference::set);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatusReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = BLOOD_PRESSURE_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int flags = 0;
        IEEE_11073_20601_SFLOAT currentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT currentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT diastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT meanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int year = 7777;
        int month = 8;
        int day = 9;
        int hours = 10;
        int minutes = 11;
        int seconds = 12;
        IEEE_11073_20601_SFLOAT pulseRate = new IEEE_11073_20601_SFLOAT(13);
        int userId = 14;
        byte[] measurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(flags
                , currentCuffPressureMmhg
                , currentCuffPressureKpa
                , diastolicUnused
                , meanArterialPressureUnused
                , year
                , month
                , day
                , hours
                , minutes
                , seconds
                , pulseRate
                , userId
                , measurementStatus);
        characteristicData.data = intermediateCuffPressure.getBytes();
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        CharacteristicData intermediateCuffPressureCharacteristicData = new CharacteristicData();
        intermediateCuffPressureCharacteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        intermediateCuffPressureCharacteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int intermediateCuffPressureFlags = BloodPressureMeasurementUtils.FLAG_BLOOD_PRESSURE_UNITS_KPA;
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueCurrentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueDiastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT intermediateCuffPressureCompoundValueMeanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int intermediateCuffPressureYear = 7777;
        int intermediateCuffPressureMonth = 8;
        int intermediateCuffPressureDay = 9;
        int intermediateCuffPressureHours = 10;
        int intermediateCuffPressureMinutes = 11;
        int intermediateCuffPressureSeconds = 12;
        IEEE_11073_20601_SFLOAT intermediateCuffPressurePulseRate = new IEEE_11073_20601_SFLOAT(13);
        int intermediateCuffPressureUserId = 14;
        byte[] intermediateCuffPressureMeasurementStatus = new byte[2];
        intermediateCuffPressure = new IntermediateCuffPressure(intermediateCuffPressureFlags
                , intermediateCuffPressureCompoundValueCurrentCuffPressureMmhg
                , intermediateCuffPressureCompoundValueCurrentCuffPressureKpa
                , intermediateCuffPressureCompoundValueDiastolicUnused
                , intermediateCuffPressureCompoundValueMeanArterialPressureUnused
                , intermediateCuffPressureYear
                , intermediateCuffPressureMonth
                , intermediateCuffPressureDay
                , intermediateCuffPressureHours
                , intermediateCuffPressureMinutes
                , intermediateCuffPressureSeconds
                , intermediateCuffPressurePulseRate
                , intermediateCuffPressureUserId
                , intermediateCuffPressureMeasurementStatus);
        intermediateCuffPressureCharacteristicData.data = intermediateCuffPressure.getBytes();
        String originalJson = mGson.toJson(intermediateCuffPressureCharacteristicData);
        mViewModel.setIntermediateCuffPressureDataJson(originalJson);

        assertTrue(hasIntermediateCuffPressureDataJsonReference.get());

        assertEquals(mFakeDeviceSettingRepository.getHexString(intermediateCuffPressureFlags, 2), intermediateCuffPressureFlagsReference.get());
        assertEquals(intermediateCuffPressureCompoundValueCurrentCuffPressureKpa.getSfloat(), Double.parseDouble(intermediateCuffPressureCurrentCuffPressureReference.get()));
        assertFalse(isIntermediateCuffPressureTimeStampSupportedReference.get());
        assertEquals("", intermediateCuffPressureTimeStampReference.get());
        assertFalse(intermediateCuffPressurePulseRateSupportedReference.get());
        assertEquals("", intermediateCuffPressurePulseRateReference.get());
        assertFalse(isIntermediateCuffPressureUserIdSupportedReference.get());
        assertEquals("", intermediateCuffPressureUserIdReference.get());
        assertFalse(isIntermediateCuffPressureMeasurementStatusSupportedReference.get());
        assertEquals("", intermediateCuffPressureMeasurementStatusReference.get());
    }

    @Test
    public void test_setIntermediateCuffPressureDataJson_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasIntermediateCuffPressureDataJsonReference = new AtomicReference<>();

        AtomicReference<String> intermediateCuffPressureFlagsReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureCurrentCuffPressureReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureTimeStampSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureTimeStampReference = new AtomicReference<>();
        AtomicReference<Boolean> intermediateCuffPressurePulseRateSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressurePulseRateReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureUserIdSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureUserIdReference = new AtomicReference<>();
        AtomicReference<Boolean> isIntermediateCuffPressureMeasurementStatusSupportedReference = new AtomicReference<>();
        AtomicReference<String> intermediateCuffPressureMeasurementStatusReference = new AtomicReference<>();

        mViewModel.observeHasIntermediateCuffPressureDataJson(new TestLifeCycleOwner(), hasIntermediateCuffPressureDataJsonReference::set);

        mViewModel.observeIntermediateCuffPressureFlags(new TestLifeCycleOwner(), intermediateCuffPressureFlagsReference::set);
        mViewModel.observeIntermediateCuffPressureCurrentCuffPressure(new TestLifeCycleOwner(), intermediateCuffPressureCurrentCuffPressureReference::set);
        mViewModel.observeIsIntermediateCuffPressureTimeStampSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureTimeStampSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureTimeStamp(new TestLifeCycleOwner(), intermediateCuffPressureTimeStampReference::set);
        mViewModel.observeIsIntermediateCuffPressurePulseRateSupported(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateSupportedReference::set);
        mViewModel.observeIntermediateCuffPressurePulseRate(new TestLifeCycleOwner(), intermediateCuffPressurePulseRateReference::set);
        mViewModel.observeIsIntermediateCuffPressureUserIdSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureUserIdSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureUserId(new TestLifeCycleOwner(), intermediateCuffPressureUserIdReference::set);
        mViewModel.observeIsIntermediateCuffPressureMeasurementStatusSupported(new TestLifeCycleOwner(), isIntermediateCuffPressureMeasurementStatusSupportedReference::set);
        mViewModel.observeIntermediateCuffPressureMeasurementStatus(new TestLifeCycleOwner(), intermediateCuffPressureMeasurementStatusReference::set);

        Intent intent = new Intent();
        ServiceData serviceData = new ServiceData();
        serviceData.uuid = BLOOD_PRESSURE_SERVICE;
        serviceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;

        CharacteristicData characteristicData = new CharacteristicData();
        characteristicData.uuid = INTERMEDIATE_CUFF_PRESSURE_CHARACTERISTIC;
        characteristicData.property = BluetoothGattCharacteristic.PROPERTY_NOTIFY;
        int flags = 0;
        IEEE_11073_20601_SFLOAT currentCuffPressureMmhg = new IEEE_11073_20601_SFLOAT(1);
        IEEE_11073_20601_SFLOAT currentCuffPressureKpa = new IEEE_11073_20601_SFLOAT(2);
        IEEE_11073_20601_SFLOAT diastolicUnused = new IEEE_11073_20601_SFLOAT(3);
        IEEE_11073_20601_SFLOAT meanArterialPressureUnused = new IEEE_11073_20601_SFLOAT(4);
        int year = 7777;
        int month = 8;
        int day = 9;
        int hours = 10;
        int minutes = 11;
        int seconds = 12;
        IEEE_11073_20601_SFLOAT pulseRate = new IEEE_11073_20601_SFLOAT(13);
        int userId = 14;
        byte[] measurementStatus = new byte[2];
        IntermediateCuffPressure intermediateCuffPressure = new IntermediateCuffPressure(flags
                , currentCuffPressureMmhg
                , currentCuffPressureKpa
                , diastolicUnused
                , meanArterialPressureUnused
                , year
                , month
                , day
                , hours
                , minutes
                , seconds
                , pulseRate
                , userId
                , measurementStatus);
        characteristicData.data = intermediateCuffPressure.getBytes();
        serviceData.characteristicDataList.add(characteristicData);
        intent.putExtra(BLOOD_PRESSURE_SERVICE.toString(), mGson.toJson(serviceData));
        mViewModel.observeSetup(intent
                , () -> {
                }
                , throwable -> {
                });

        mViewModel.setIntermediateCuffPressureDataJson(null);

        assertFalse(hasIntermediateCuffPressureDataJsonReference.get());

        assertEquals("", intermediateCuffPressureFlagsReference.get());
        assertEquals("", intermediateCuffPressureCurrentCuffPressureReference.get());
        assertFalse(isIntermediateCuffPressureTimeStampSupportedReference.get());
        assertEquals("", intermediateCuffPressureTimeStampReference.get());
        assertFalse(intermediateCuffPressurePulseRateSupportedReference.get());
        assertEquals("", intermediateCuffPressurePulseRateReference.get());
        assertFalse(isIntermediateCuffPressureUserIdSupportedReference.get());
        assertEquals("", intermediateCuffPressureUserIdReference.get());
        assertFalse(isIntermediateCuffPressureMeasurementStatusSupportedReference.get());
        assertEquals("", intermediateCuffPressureMeasurementStatusReference.get());
    }

}