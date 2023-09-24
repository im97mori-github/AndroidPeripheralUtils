package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;
import static org.junit.Assert.assertArrayEquals;

import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Collections;
import java.util.LinkedList;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
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
public class BloodPressureProfileViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private SavedStateHandle mSavedStateHandle;

    private BloodPressureProfileViewModel mViewModel;

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mViewModel = new BloodPressureProfileViewModel(mSavedStateHandle
                , new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext));
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
        mSavedStateHandle = null;
    }

    @Test
    public void test_observeSetup_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsData = new AtomicReference<>();
        AtomicReference<Boolean> hasDisData = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsData(new TestLifeCycleOwner(), hasBlsData::set);
        mViewModel.observeHasDisData(new TestLifeCycleOwner(), hasDisData::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData(new LinkedList<>());
        byte[] original = Utils.parcelableToByteArray(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertNull(hasBlsData.get());
        assertNull(hasDisData.get());
        assertFalse(isDisSupported.get());
    }

    @Test
    public void test_observeSetup_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsData = new AtomicReference<>();
        AtomicReference<Boolean> hasDisData = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsData(new TestLifeCycleOwner(), hasBlsData::set);
        mViewModel.observeHasDisData(new TestLifeCycleOwner(), hasDisData::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData(new LinkedList<>());
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        byte[] original = Utils.parcelableToByteArray(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertNotNull(hasBlsData.get());
        assertNull(hasDisData.get());
        assertFalse(isDisSupported.get());
    }

    @Test
    public void test_observeSetup_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsData = new AtomicReference<>();
        AtomicReference<Boolean> hasDisData = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsData(new TestLifeCycleOwner(), hasBlsData::set);
        mViewModel.observeHasDisData(new TestLifeCycleOwner(), hasDisData::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData(new LinkedList<>());
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        mockData.serviceDataList.add(new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        byte[] original = Utils.parcelableToByteArray(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertNotNull(hasBlsData.get());
        assertNotNull(hasDisData.get());
        assertTrue(isDisSupported.get());
    }

    @Test
    public void test_observeSetup_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean originalIsDisSupport = true;
        String originalBlsData = "a";
        String originalDisData = "b";

        mSavedStateHandle.set("KEY_IS_DIS_SUPPORTED", originalIsDisSupport);
        mSavedStateHandle.set("KEY_BLS_DATA", originalBlsData);
        mSavedStateHandle.set("KEY_DIS_DATA", originalDisData);

        AtomicReference<Boolean> hasBlsData = new AtomicReference<>();
        AtomicReference<Boolean> hasDisData = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsData(new TestLifeCycleOwner(), hasBlsData::set);
        mViewModel.observeHasDisData(new TestLifeCycleOwner(), hasDisData::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData(new LinkedList<>());
        byte[] original = Utils.parcelableToByteArray(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertTrue(hasBlsData.get());
        assertTrue(hasDisData.get());
        assertTrue(isDisSupported.get());
    }

    @Test
    public void test_observeSaveData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<byte[]> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);

        assertNull(saveDataReference.get());
    }

    @Test
    public void test_observeSaveData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData(new LinkedList<>());
        byte[] original = Utils.parcelableToByteArray(mockData);
        AtomicReference<byte[]> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);
        mSavedStateHandle.set("KEY_SAVED_DATA", original);

        assertArrayEquals(original, saveDataReference.get());
    }

    @Test
    public void test_save_1_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData(new LinkedList<>());
        byte[] original = Utils.parcelableToByteArray(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.setDisData(new byte[0]);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData(new LinkedList<>());
        byte[] original = Utils.parcelableToByteArray(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.setBlsData(new byte[0]);
        mViewModel.updateIsDisSupported(true);

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData(new LinkedList<>());
        ServiceData blsData = new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());
        mockData.serviceDataList.add(blsData);
        byte[] original = Utils.parcelableToByteArray(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });

        AtomicReference<byte[]> mockDataStringReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), mockDataStringReference::set);
        mViewModel.save(throwable -> {
        });

        MockData savedMockData = Utils.byteToParcelable(mockDataStringReference.get(), MockData.CREATOR);
        assertNotNull(savedMockData);

        Optional<ServiceData> blsServiceDataOptional = savedMockData.serviceDataList
                .stream()
                .filter(serviceData -> serviceData.uuid.equals(BLOOD_PRESSURE_SERVICE))
                .findAny();
        assertTrue(blsServiceDataOptional.isPresent());
        assertEquals(blsData, blsServiceDataOptional.get());
    }

    @Test
    public void test_save_2_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData(new LinkedList<>());
        ServiceData blsData = new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());
        ServiceData disData = new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, new LinkedList<>());
        mockData.serviceDataList.add(blsData);
        mockData.serviceDataList.add(disData);
        byte[] original = Utils.parcelableToByteArray(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateIsDisSupported(true);

        AtomicReference<byte[]> mockDataStringReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), mockDataStringReference::set);
        mViewModel.save(throwable -> {
        });

        MockData savedMockData = Utils.byteToParcelable(mockDataStringReference.get(), MockData.CREATOR);
        assertNotNull(savedMockData);

        Optional<ServiceData> blsServiceDataOptional = savedMockData.serviceDataList
                .stream()
                .filter(serviceData -> serviceData.uuid.equals(BLOOD_PRESSURE_SERVICE))
                .findAny();
        assertTrue(blsServiceDataOptional.isPresent());
        assertEquals(blsData, blsServiceDataOptional.get());

        Optional<ServiceData> disServiceDataOptional = savedMockData.serviceDataList
                .stream()
                .filter(serviceData -> serviceData.uuid.equals(DEVICE_INFORMATION_SERVICE))
                .findAny();
        assertTrue(disServiceDataOptional.isPresent());
        assertEquals(disData, disServiceDataOptional.get());
    }

    @Test
    public void test_observeHasBlsData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsData = new AtomicReference<>();

        mViewModel.observeHasBlsData(new TestLifeCycleOwner(), hasBlsData::set);

        mViewModel.setBlsData(new byte[0]);

        assertTrue(hasBlsData.get());
    }

    @Test
    public void test_observeHasBlsData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsData = new AtomicReference<>();

        mViewModel.observeHasBlsData(new TestLifeCycleOwner(), hasBlsData::set);

        mViewModel.setBlsData(null);

        assertFalse(hasBlsData.get());
    }

    @Test
    public void test_observeHasDisData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasDisData = new AtomicReference<>();

        mViewModel.observeHasDisData(new TestLifeCycleOwner(), hasDisData::set);

        mViewModel.setDisData(new byte[0]);

        assertTrue(hasDisData.get());
    }

    @Test
    public void test_observeHasDisData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasDisData = new AtomicReference<>();

        mViewModel.observeHasDisData(new TestLifeCycleOwner(), hasDisData::set);

        mViewModel.setDisData(null);

        assertFalse(hasDisData.get());
    }

    @Test
    public void test_observeIsDisSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        mViewModel.updateIsDisSupported(true);

        assertTrue(isDisSupported.get());
    }

    @Test
    public void test_observeIsDisSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        mViewModel.updateIsDisSupported(false);

        assertFalse(isDisSupported.get());
    }

    @Test
    public void test_updateIsDisSupported_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mViewModel.updateIsDisSupported(true);

        Boolean isDisSupported = mSavedStateHandle.get("KEY_IS_DIS_SUPPORTED");
        assertNotNull(isDisSupported);
        assertTrue(isDisSupported);
    }

    @Test
    public void test_updateIsDisSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mViewModel.updateIsDisSupported(false);

        Boolean isDisSupported = mSavedStateHandle.get("KEY_IS_DIS_SUPPORTED");
        assertNotNull(isDisSupported);
        assertFalse(isDisSupported);
    }

    @Test
    public void test_getBlsData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[]{1};
        mSavedStateHandle.set("KEY_BLS_DATA", original);

        assertArrayEquals(original, mViewModel.getBlsData());
    }

    @Test
    public void test_setBlsData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[]{1};
        mViewModel.setBlsData(original);

        assertEquals(original, mSavedStateHandle.get("KEY_BLS_DATA"));
    }

    @Test
    public void test_getDisData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[]{1};
        mSavedStateHandle.set("KEY_DIS_DATA", original);

        assertEquals(original, mViewModel.getDisData());
    }

    @Test
    public void test_setDisData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        byte[] original = new byte[]{1};
        mViewModel.setDisData(original);

        assertEquals(original, mSavedStateHandle.get("KEY_DIS_DATA"));
    }

}