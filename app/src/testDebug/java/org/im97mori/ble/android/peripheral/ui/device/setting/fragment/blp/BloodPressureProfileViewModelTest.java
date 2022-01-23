package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Collections;
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

@SuppressWarnings("ConstantConditions")
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

    @Inject
    Gson mGson;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mViewModel = new BloodPressureProfileViewModel(mSavedStateHandle
                , new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext)
                , mGson);
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

        AtomicReference<Boolean> hasBlsDataJson = new AtomicReference<>();
        AtomicReference<Boolean> hasDisDataJson = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsDataJson(new TestLifeCycleOwner(), hasBlsDataJson::set);
        mViewModel.observeHasDisDataJson(new TestLifeCycleOwner(), hasDisDataJson::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData();
        String original = mGson.toJson(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertNull(hasBlsDataJson.get());
        assertNull(hasDisDataJson.get());
        assertFalse(isDisSupported.get());
    }

    @Test
    public void test_observeSetup_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsDataJson = new AtomicReference<>();
        AtomicReference<Boolean> hasDisDataJson = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsDataJson(new TestLifeCycleOwner(), hasBlsDataJson::set);
        mViewModel.observeHasDisDataJson(new TestLifeCycleOwner(), hasDisDataJson::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        String original = mGson.toJson(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertNotNull(hasBlsDataJson.get());
        assertNull(hasDisDataJson.get());
        assertFalse(isDisSupported.get());
    }

    @Test
    public void test_observeSetup_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsDataJson = new AtomicReference<>();
        AtomicReference<Boolean> hasDisDataJson = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsDataJson(new TestLifeCycleOwner(), hasBlsDataJson::set);
        mViewModel.observeHasDisDataJson(new TestLifeCycleOwner(), hasDisDataJson::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        mockData.serviceDataList.add(new ServiceData(DEVICE_INFORMATION_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        String original = mGson.toJson(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertNotNull(hasBlsDataJson.get());
        assertNotNull(hasDisDataJson.get());
        assertTrue(isDisSupported.get());
    }

    @Test
    public void test_observeSetup_2_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean originalIsDisSupport = true;
        String originalBlsDataJson = "a";
        String originalDisDataJson = "b";

        mSavedStateHandle.set("KEY_IS_DIS_SUPPORTED", originalIsDisSupport);
        mSavedStateHandle.set("KEY_BLS_DATA_JSON", originalBlsDataJson);
        mSavedStateHandle.set("KEY_DIS_DATA_JSON", originalDisDataJson);

        AtomicReference<Boolean> hasBlsDataJson = new AtomicReference<>();
        AtomicReference<Boolean> hasDisDataJson = new AtomicReference<>();
        AtomicReference<Boolean> isDisSupported = new AtomicReference<>();

        mViewModel.observeHasBlsDataJson(new TestLifeCycleOwner(), hasBlsDataJson::set);
        mViewModel.observeHasDisDataJson(new TestLifeCycleOwner(), hasDisDataJson::set);
        mViewModel.observeIsDisSupported(new TestLifeCycleOwner(), isDisSupported::set);

        MockData mockData = new MockData();
        String original = mGson.toJson(mockData);
        AtomicBoolean result = new AtomicBoolean(false);

        mViewModel.observeSetup(original, () -> result.set(true), throwable -> {
        });

        assertTrue(result.get());

        assertTrue(hasBlsDataJson.get());
        assertTrue(hasDisDataJson.get());
        assertTrue(isDisSupported.get());
    }

    @Test
    public void test_observeSaveData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> saveDataReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), saveDataReference::set);

        assertNull(saveDataReference.get());
    }

    @Test
    public void test_observeSaveData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData();
        String original = mGson.toJson(mockData);
        AtomicReference<String> saveDataReference = new AtomicReference<>();
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
        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData();
        String original = mGson.toJson(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.setDisDataJson("");

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.save(throwableReference::set);

        assertNotNull(throwableReference.get());

        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_save_1_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData mockData = new MockData();
        String original = mGson.toJson(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.setBlsDataJson("");
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

        MockData mockData = new MockData();
        ServiceData blsData = new ServiceData();
        blsData.uuid = BLOOD_PRESSURE_SERVICE;
        mockData.serviceDataList.add(blsData);
        String original = mGson.toJson(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });

        AtomicReference<String> mockDataStringReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), mockDataStringReference::set);
        mViewModel.save(throwable -> {
        });

        MockData savedMockData = mGson.fromJson(mockDataStringReference.get(), MockData.class);
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

        MockData mockData = new MockData();
        ServiceData blsData = new ServiceData();
        blsData.uuid = BLOOD_PRESSURE_SERVICE;
        ServiceData disData = new ServiceData();
        disData.uuid = DEVICE_INFORMATION_SERVICE;
        mockData.serviceDataList.add(blsData);
        mockData.serviceDataList.add(disData);
        String original = mGson.toJson(mockData);
        mViewModel.observeSetup(original
                , () -> {
                }
                , throwable -> {
                });
        mViewModel.updateIsDisSupported(true);

        AtomicReference<String> mockDataStringReference = new AtomicReference<>();
        mViewModel.observeSavedData(new TestLifeCycleOwner(), mockDataStringReference::set);
        mViewModel.save(throwable -> {
        });

        MockData savedMockData = mGson.fromJson(mockDataStringReference.get(), MockData.class);
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
    public void test_observeHasBlsDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsDataJson = new AtomicReference<>();

        mViewModel.observeHasBlsDataJson(new TestLifeCycleOwner(), hasBlsDataJson::set);

        mViewModel.setBlsDataJson("");

        assertTrue(hasBlsDataJson.get());
    }

    @Test
    public void test_observeHasBlsDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasBlsDataJson = new AtomicReference<>();

        mViewModel.observeHasBlsDataJson(new TestLifeCycleOwner(), hasBlsDataJson::set);

        mViewModel.setBlsDataJson(null);

        assertFalse(hasBlsDataJson.get());
    }

    @Test
    public void test_observeHasDisDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasDisDataJson = new AtomicReference<>();

        mViewModel.observeHasDisDataJson(new TestLifeCycleOwner(), hasDisDataJson::set);

        mViewModel.setDisDataJson("");

        assertTrue(hasDisDataJson.get());
    }

    @Test
    public void test_observeHasDisDataJson_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> hasDisDataJson = new AtomicReference<>();

        mViewModel.observeHasDisDataJson(new TestLifeCycleOwner(), hasDisDataJson::set);

        mViewModel.setDisDataJson(null);

        assertFalse(hasDisDataJson.get());
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

        assertTrue(mSavedStateHandle.get("KEY_IS_DIS_SUPPORTED"));
    }

    @Test
    public void test_updateIsDisSupported_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mViewModel.updateIsDisSupported(false);

        assertFalse(mSavedStateHandle.get("KEY_IS_DIS_SUPPORTED"));
    }

    @Test
    public void test_getBlsDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mSavedStateHandle.set("KEY_BLS_DATA_JSON", original);

        assertEquals(original, mViewModel.getBlsDataJson());
    }

    @Test
    public void test_setBlsDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mViewModel.setBlsDataJson(original);

        assertEquals(original, mSavedStateHandle.get("KEY_BLS_DATA_JSON"));
    }

    @Test
    public void test_getDisDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mSavedStateHandle.set("KEY_DIS_DATA_JSON", original);

        assertEquals(original, mViewModel.getDisDataJson());
    }

    @Test
    public void test_setDisDataJson_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mViewModel.setDisDataJson(original);

        assertEquals(original, mSavedStateHandle.get("KEY_DIS_DATA_JSON"));
    }

}