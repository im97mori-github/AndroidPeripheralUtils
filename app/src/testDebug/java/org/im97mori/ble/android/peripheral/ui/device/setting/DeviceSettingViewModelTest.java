package org.im97mori.ble.android.peripheral.ui.device.setting;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;

import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.BaseSettingFragmentViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileFragment;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileViewModel;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Collections;
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
import io.reactivex.rxjava3.processors.PublishProcessor;
import io.reactivex.rxjava3.schedulers.Schedulers;

@SuppressWarnings("ConstantConditions")
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceSettingViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Gson mGson;

    private DeviceSettingViewModel mViewModel;

    private SavedStateHandle mSavedStateHandle;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new DeviceSettingViewModel(mSavedStateHandle, mFakeDeviceSettingRepository, mGson);
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
        mSavedStateHandle = null;
    }

    @Test
    public void test_observeSetup_3_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();
        AtomicReference<String> deviceTypeNameReference = new AtomicReference<>();
        AtomicReference<String> deviceSettingNameReference = new AtomicReference<>();
        AtomicReference<String> deviceSettingNameErrorStringReference = new AtomicReference<>();
        AtomicReference<MockData> mockDataReference = new AtomicReference<>();

        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeNameReference::set);
        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), deviceSettingNameReference::set);
        mViewModel.observeDeviceSettingNameErrorString(new TestLifeCycleOwner(), deviceSettingNameErrorStringReference::set);
        mViewModel.observeMockData(mockDataReference::set);

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });

        assertEquals(R.drawable.medical_ketsuatsukei_aneroid, deviceTypeImageResIdReference.get().intValue());
        assertEquals(mContext.getString(R.string.blood_pressure_profile), deviceTypeNameReference.get());
        assertEquals("", deviceSettingNameReference.get());
        assertEquals(mContext.getString(R.string.no_value), deviceSettingNameErrorStringReference.get());
        assertNotNull(mockDataReference.get());
    }

    @Test
    public void test_observeSetup_5_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        DeviceSetting original = new DeviceSetting(2, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);

        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();
        AtomicReference<String> deviceTypeNameReference = new AtomicReference<>();
        AtomicReference<String> deviceSettingNameReference = new AtomicReference<>();
        AtomicReference<String> deviceSettingNameErrorStringReference = new AtomicReference<>();
        AtomicReference<MockData> mockDataReference = new AtomicReference<>();

        AtomicReference<Throwable> observeSetupThrowable = new AtomicReference<>();
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();
        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeNameReference::set);
        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), deviceSettingNameReference::set);
        mViewModel.observeDeviceSettingNameErrorString(new TestLifeCycleOwner(), deviceSettingNameErrorStringReference::set);
        mViewModel.observeMockData(mockDataReference::set);

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_ID, original.getId());
        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertNull(observeSetupThrowable.get());

        assertEquals(R.drawable.medical_ketsuatsukei_aneroid, deviceTypeImageResIdReference.get().intValue());
        assertEquals(mContext.getString(R.string.blood_pressure_profile), deviceTypeNameReference.get());
        assertEquals(original.getDeviceSettingName(), deviceSettingNameReference.get());
        assertNull(deviceSettingNameErrorStringReference.get());
        assertNotNull(mockDataReference.get());
    }

    @Test
    public void test_observeSetup_5_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData originalMockData = new MockData();
        ServiceData originalServiceData = new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList());
        originalMockData.serviceDataList.add(originalServiceData);
        DeviceSetting original = new DeviceSetting(2, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(originalMockData));

        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();
        AtomicReference<String> deviceTypeNameReference = new AtomicReference<>();
        AtomicReference<String> deviceSettingNameReference = new AtomicReference<>();
        AtomicReference<String> deviceSettingNameErrorStringReference = new AtomicReference<>();
        AtomicReference<MockData> mockDataReference = new AtomicReference<>();

        AtomicReference<Throwable> observeSetupThrowable = new AtomicReference<>();
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();
        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeNameReference::set);
        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), deviceSettingNameReference::set);
        mViewModel.observeDeviceSettingNameErrorString(new TestLifeCycleOwner(), deviceSettingNameErrorStringReference::set);
        mViewModel.observeMockData(mockDataReference::set);

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_ID, original.getId());
        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertNull(observeSetupThrowable.get());

        assertEquals(R.drawable.medical_ketsuatsukei_aneroid, deviceTypeImageResIdReference.get().intValue());
        assertEquals(mContext.getString(R.string.blood_pressure_profile), deviceTypeNameReference.get());
        assertEquals(original.getDeviceSettingName(), deviceSettingNameReference.get());
        assertNull(deviceSettingNameErrorStringReference.get());
        MockData mockData = mockDataReference.get();
        assertNotNull(mockData);
        assertEquals(1, mockData.serviceDataList.size());
        assertEquals(originalServiceData, mockData.serviceDataList.get(0));
    }

    @Test
    public void test_observeSetup_6_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData originalMockData = new MockData();
        ServiceData originalServiceData = new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList());
        originalMockData.serviceDataList.add(originalServiceData);
        DeviceSetting original = new DeviceSetting(2, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(originalMockData));

        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", R.drawable.medical_ketsuatsukei_aneroid);
        mSavedStateHandle.set("KEY_DEVICE_TYPE_NAME", mContext.getString(R.string.blood_pressure_profile));
        mSavedStateHandle.set("KEY_DEVICE_SETTING_NAME", original.getDeviceSettingName());

        AtomicInteger deviceTypeImageResIdCount = new AtomicInteger(0);
        AtomicInteger deviceTypeNameCount = new AtomicInteger(0);
        AtomicInteger deviceSettingNameCount = new AtomicInteger(0);
        AtomicInteger deviceSettingNameErrorStringCount = new AtomicInteger(0);
        AtomicReference<MockData> mockDataReference = new AtomicReference<>();

        AtomicReference<Throwable> observeSetupThrowable = new AtomicReference<>();
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();
        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), integer -> deviceTypeImageResIdCount.incrementAndGet());
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), s -> deviceTypeNameCount.incrementAndGet());
        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), s -> deviceSettingNameCount.incrementAndGet());
        mViewModel.observeDeviceSettingNameErrorString(new TestLifeCycleOwner(), s -> deviceSettingNameErrorStringCount.incrementAndGet());
        mViewModel.observeMockData(mockDataReference::set);

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_ID, original.getId());
        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertNull(observeSetupThrowable.get());

        assertEquals(1, deviceTypeImageResIdCount.get());
        assertEquals(1, deviceTypeNameCount.get());
        assertEquals(1, deviceSettingNameCount.get());
        assertEquals(1, deviceSettingNameErrorStringCount.get());

        MockData mockData = mockDataReference.get();
        assertNotNull(mockData);
        assertEquals(1, mockData.serviceDataList.size());
        assertEquals(originalServiceData, mockData.serviceDataList.get(0));
    }

    @Test
    public void test_observeDeviceTypeImageResId_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();

        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);

        assertNull(deviceTypeImageResIdReference.get());
    }

    @Test
    public void test_observeDeviceTypeImageResId_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();

        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);
        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", original);

        assertEquals(original, deviceTypeImageResIdReference.get().intValue());
    }

    @Test
    public void test_observeDeviceTypeImageResId_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", original);
        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);

        assertEquals(original, deviceTypeImageResIdReference.get().intValue());
    }

    @Test
    public void test_observeDeviceTypeImageResId_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();

        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), integer -> {
            count.incrementAndGet();
            deviceTypeImageResIdReference.set(integer);
        });
        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", original);
        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", original);

        assertEquals(original, deviceTypeImageResIdReference.get().intValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeDeviceTypeName_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeName::set);

        assertNull(deviceTypeName.get());
    }

    @Test
    public void test_observeDeviceTypeName_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeName::set);
        mSavedStateHandle.set("KEY_DEVICE_TYPE_NAME", original);

        assertEquals(original, deviceTypeName.get());
    }

    @Test
    public void test_observeDeviceTypeName_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE_NAME", original);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeName::set);

        assertEquals(original, deviceTypeName.get());
    }

    @Test
    public void test_observeDeviceTypeName_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            deviceTypeName.set(s);
        });
        mSavedStateHandle.set("KEY_DEVICE_TYPE_NAME", original);
        mSavedStateHandle.set("KEY_DEVICE_TYPE_NAME", original);

        assertEquals(original, deviceTypeName.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeDeviceSettingName_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> deviceSettingName = new AtomicReference<>();

        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), deviceSettingName::set);

        assertNull(deviceSettingName.get());
    }

    @Test
    public void test_observeDeviceSettingName_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> deviceSettingName = new AtomicReference<>();

        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), deviceSettingName::set);
        mViewModel.updateDeviceSettingName(original);

        assertEquals(original, deviceSettingName.get());
    }

    @Test
    public void test_observeDeviceSettingName_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> deviceSettingName = new AtomicReference<>();

        mViewModel.updateDeviceSettingName(original);
        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), deviceSettingName::set);

        assertEquals(original, deviceSettingName.get());
    }

    @Test
    public void test_observeDeviceSettingName_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mViewModel.observeDeviceSettingName(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            deviceTypeName.set(s);
        });
        mViewModel.updateDeviceSettingName(original);
        mViewModel.updateDeviceSettingName(original);

        assertEquals(original, deviceTypeName.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeDeviceSettingNameErrorString_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mGetDeviceSettingNameErrorString = "a";
        AtomicReference<String> deviceSettingNameErrorString = new AtomicReference<>();

        mViewModel.observeDeviceSettingNameErrorString(new TestLifeCycleOwner(), deviceSettingNameErrorString::set);

        assertNull(deviceSettingNameErrorString.get());
    }

    @Test
    public void test_observeDeviceSettingNameErrorString_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        mFakeDeviceSettingRepository.mGetDeviceSettingNameErrorString = original;
        AtomicReference<String> deviceSettingNameErrorString = new AtomicReference<>();

        mViewModel.observeDeviceSettingNameErrorString(new TestLifeCycleOwner(), deviceSettingNameErrorString::set);
        mViewModel.updateDeviceSettingName(null);

        assertEquals(original, deviceSettingNameErrorString.get());
    }

    @Test
    public void test_observeDeviceSettingNameErrorString_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        mFakeDeviceSettingRepository.mGetDeviceSettingNameErrorString = original;
        AtomicReference<String> deviceSettingNameErrorString = new AtomicReference<>();

        mViewModel.observeDeviceSettingNameErrorString(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            deviceSettingNameErrorString.set(s);
        });
        mViewModel.updateDeviceSettingName(null);
        mViewModel.updateDeviceSettingName(null);

        assertEquals(original, deviceSettingNameErrorString.get());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeFragmentReady_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> fragmentReady = new AtomicReference<>();

        mViewModel.observeFragmentReady(new TestLifeCycleOwner(), fragmentReady::set);

        assertNull(fragmentReady.get());
    }

    @Test
    public void test_observeFragmentReady_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> fragmentReady = new AtomicReference<>();

        mViewModel.observeFragmentReady(new TestLifeCycleOwner(), fragmentReady::set);
        mSavedStateHandle.set("KEY_FRAGMENT_READY", original);

        assertEquals(original, fragmentReady.get().booleanValue());
    }

    @Test
    public void test_observeFragmentReady_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicReference<Boolean> fragmentReady = new AtomicReference<>();

        mSavedStateHandle.set("KEY_FRAGMENT_READY", original);
        mViewModel.observeFragmentReady(new TestLifeCycleOwner(), fragmentReady::set);

        assertEquals(original, fragmentReady.get().booleanValue());
    }

    @Test
    public void test_observeFragmentReady_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        boolean original = true;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Boolean> fragmentReady = new AtomicReference<>();

        mViewModel.observeFragmentReady(new TestLifeCycleOwner(), b -> {
            count.incrementAndGet();
            fragmentReady.set(b);
        });
        mSavedStateHandle.set("KEY_FRAGMENT_READY", original);
        mSavedStateHandle.set("KEY_FRAGMENT_READY", original);

        assertEquals(original, fragmentReady.get().booleanValue());
        assertEquals(1, count.get());
    }

    @Test
    public void test_observeMockData_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<MockData> mockDataReference = new AtomicReference<>();

        mViewModel.observeMockData(mockDataReference::set);

        assertNull(mockDataReference.get());
    }

    @Test
    public void test_observeMockData_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        MockData originalMockData = new MockData();
        ServiceData originalServiceData = new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList());
        originalMockData.serviceDataList.add(originalServiceData);
        DeviceSetting original = new DeviceSetting(2, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(originalMockData));

        AtomicReference<MockData> mockDataReference = new AtomicReference<>();
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeMockData(mockDataReference::set);

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_ID, original.getId());
        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        MockData mockData = mockDataReference.get();
        assertNotNull(mockData);
        assertEquals(1, mockData.serviceDataList.size());
        assertEquals(originalServiceData, mockData.serviceDataList.get(0));
    }

    @Test
    public void test_updateDeviceSettingName_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String after = "b";

        assertNull(mSavedStateHandle.get("KEY_DEVICE_SETTING_NAME"));
        mViewModel.updateDeviceSettingName(after);

        assertEquals(after, mSavedStateHandle.get("KEY_DEVICE_SETTING_NAME"));
    }

    @Test
    public void test_updateDeviceSettingName_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String before = "a";
        String after = "b";

        mViewModel.updateDeviceSettingName(before);
        assertEquals(before, mSavedStateHandle.get("KEY_DEVICE_SETTING_NAME"));

        mViewModel.updateDeviceSettingName(after);

        assertEquals(after, mSavedStateHandle.get("KEY_DEVICE_SETTING_NAME"));
    }

    @Test
    public void test_fragmentReady_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        assertNull(mSavedStateHandle.get("KEY_FRAGMENT_READY"));
        mViewModel.fragmentReady();

        assertTrue(mSavedStateHandle.get("KEY_FRAGMENT_READY"));
    }

    @Test
    public void test_getFragment_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        Throwable t = null;
        try {
            mViewModel.getFragment(intent);
        } catch (Exception e) {
            t = e;
        }

        assertNotNull(t);
        assertEquals("No Data", t.getMessage());
    }

    @Test
    public void test_getFragment_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED);
        Throwable t = null;
        try {
            mViewModel.getFragment(intent);
        } catch (Exception e) {
            t = e;
        }

        assertNotNull(t);
        assertEquals("No Data", t.getMessage());
    }

    @Test
    public void test_getFragment_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        Fragment baseDeviceSettingFragment = mViewModel.getFragment(intent);

        assertTrue(baseDeviceSettingFragment instanceof BloodPressureProfileFragment);
    }

    @Test
    public void test_getFragmentViewModelClass_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        Throwable t = null;
        try {
            mViewModel.getFragmentViewModelClass(intent);
        } catch (Exception e) {
            t = e;
        }

        assertNotNull(t);
        assertEquals("No Data", t.getMessage());
    }

    @Test
    public void test_getFragmentViewModelClass_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED);
        Throwable t = null;
        try {
            mViewModel.getFragmentViewModelClass(intent);
        } catch (Exception e) {
            t = e;
        }

        assertNotNull(t);
        assertEquals("No Data", t.getMessage());
    }

    @Test
    public void test_getFragmentViewModelClass_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        Class<? extends BaseSettingFragmentViewModel> clazz = mViewModel.getFragmentViewModelClass(intent);

        assertEquals(BloodPressureProfileViewModel.class, clazz);
    }

    @Test
    public void test_observeSave_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(() -> null, () -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Already saved", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        DeviceSetting original = new DeviceSetting(2, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        intent.putExtra(KEY_DEVICE_ID, original.getId());
        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(() -> null, () -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        DeviceSetting original = new DeviceSetting(2, "", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        intent.putExtra(KEY_DEVICE_ID, original.getId());
        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        AtomicReference<Throwable> throwableReference = new AtomicReference<>();
        mViewModel.observeSave(() -> "", () -> {
        }, throwableReference::set);

        assertNotNull(throwableReference.get());
        assertEquals("Validation failed", throwableReference.get().getMessage());
    }

    @Test
    public void test_observeSave_00005() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);
        AtomicReference<DeviceSetting> deviceSettingAtomicReference = new AtomicReference<>();
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();
        mFakeDeviceSettingRepository.mInsertDeviceSettingConsumer = deviceSettingAtomicReference::set;

        DeviceSetting original = new DeviceSetting(2, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null);
        String mockDataString = "b";

        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_ID, original.getId());
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);

        mViewModel.observeSetup(intent, () -> {
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        mViewModel.observeSave(() -> mockDataString
                , () -> result.set(true)
                , throwable -> {
                });

        assertTrue(result.get());

        DeviceSetting deviceSetting = deviceSettingAtomicReference.get();
        assertNotNull(deviceSetting);
        assertEquals(original.getId(), deviceSetting.getId());
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(mockDataString, deviceSetting.getDeviceSettingData());
    }

}