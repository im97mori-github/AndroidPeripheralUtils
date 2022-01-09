package org.im97mori.ble.android.peripheral.ui.device;

import static junit.framework.TestCase.assertNull;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.content.Intent;
import android.os.Build;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import junit.framework.TestCase;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeBluetoothSettingRepository;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
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

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class PeripheralViewModelTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private FakeBluetoothSettingRepository mFakeBluetoothSettingRepository;

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    Gson mGson;

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private PeripheralViewModel mViewModel;

    private SavedStateHandle mSavedStateHandle;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mSavedStateHandle = new SavedStateHandle();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mFakeBluetoothSettingRepository = new FakeBluetoothSettingRepository(mContext);
        mViewModel = new PeripheralViewModel(mSavedStateHandle
                , mFakeDeviceSettingRepository
                , mFakeBluetoothSettingRepository
                , mGson);
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
        mSavedStateHandle = null;
        mFakeDeviceSettingRepository = null;
        mFakeBluetoothSettingRepository = null;
    }

    @Test
    public void test_observeSetup_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();
        AtomicReference<String> titleReference = new AtomicReference<>();
        AtomicReference<String> deviceTypeNameReference = new AtomicReference<>();
        AtomicReference<Integer> deviceTypeReference = new AtomicReference<>();
        AtomicReference<Boolean> isReadyReference = new AtomicReference<>();
        AtomicReference<Boolean> isStartedReference = new AtomicReference<>();

        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);
        mViewModel.observeTitle(new TestLifeCycleOwner(), titleReference::set);
        mViewModel.observeDeviceType(new TestLifeCycleOwner(), deviceTypeReference::set);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeNameReference::set);
        mViewModel.observeIsReady(new TestLifeCycleOwner(), isReadyReference::set);
        mViewModel.observeIsStarted(new TestLifeCycleOwner(), isStartedReference::set);

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> result.set(true)
                , throwable -> {
                });

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertEquals(R.drawable.medical_ketsuatsukei_aneroid, deviceTypeImageResIdReference.get().intValue());
        assertEquals(original.getDeviceSettingName(), titleReference.get());
        assertEquals(mContext.getString(R.string.blood_pressure_profile), deviceTypeNameReference.get());
        assertEquals(original.getDeviceType(), deviceTypeReference.get().intValue());
        assertTrue(isReadyReference.get());
        assertFalse(isStartedReference.get());

        assertTrue(result.get());
    }

    @Test
    public void test_observeSetup_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();
        AtomicReference<String> titleReference = new AtomicReference<>();
        AtomicReference<String> deviceTypeNameReference = new AtomicReference<>();
        AtomicReference<Integer> deviceTypeReference = new AtomicReference<>();
        AtomicReference<Boolean> isReadyReference = new AtomicReference<>();
        AtomicReference<Boolean> isStartedReference = new AtomicReference<>();

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> {
                }
                , throwable -> {
                });

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);
        mViewModel.observeTitle(new TestLifeCycleOwner(), titleReference::set);
        mViewModel.observeDeviceType(new TestLifeCycleOwner(), deviceTypeReference::set);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeNameReference::set);
        mViewModel.observeIsReady(new TestLifeCycleOwner(), isReadyReference::set);
        mViewModel.observeIsStarted(new TestLifeCycleOwner(), isStartedReference::set);

        mViewModel.observeSetup(new Intent()
                , () -> result.set(true)
                , throwable -> {
                });

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertEquals(R.drawable.medical_ketsuatsukei_aneroid, deviceTypeImageResIdReference.get().intValue());
        assertEquals(original.getDeviceSettingName(), titleReference.get());
        assertEquals(mContext.getString(R.string.blood_pressure_profile), deviceTypeNameReference.get());
        assertEquals(original.getDeviceType(), deviceTypeReference.get().intValue());
        assertTrue(isReadyReference.get());
        assertFalse(isStartedReference.get());

        assertTrue(result.get());
    }

    @Test
    public void test_observeSetup_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> result.set(true)
                , throwable -> {
                });

        mFakeBluetoothSettingRepository.mAddBluetoothStatusConsumerAction = () -> result.set(true);

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertTrue(result.get());
    }

    @Test
    public void test_observeSetup_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> {
                }
                , throwable -> {
                });

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        mFakeBluetoothSettingRepository.mAddBluetoothStatusConsumerAction = () -> result.set(true);

        mViewModel.observeSetup(new Intent()
                , () -> result.set(true)
                , throwable -> {
                });

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();


        assertTrue(result.get());
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

        TestCase.assertEquals(original, deviceTypeImageResIdReference.get().intValue());
    }

    @Test
    public void test_observeDeviceTypeImageResId_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", original);
        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), deviceTypeImageResIdReference::set);

        TestCase.assertEquals(original, deviceTypeImageResIdReference.get().intValue());
    }

    @Test
    public void test_observeDeviceTypeImageResId_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Integer> deviceTypeImageResIdReference = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE_IMAGE_RES_ID", original);
        mViewModel.observeDeviceTypeImageResId(new TestLifeCycleOwner(), integer -> {
            count.incrementAndGet();
            deviceTypeImageResIdReference.set(integer);
        });

        TestCase.assertEquals(original, deviceTypeImageResIdReference.get().intValue());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_title_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<String> title = new AtomicReference<>();

        mViewModel.observeTitle(new TestLifeCycleOwner(), title::set);

        assertNull(title.get());
    }

    @Test
    public void test_title_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> title = new AtomicReference<>();

        mViewModel.observeTitle(new TestLifeCycleOwner(), title::set);
        mSavedStateHandle.set("KEY_TITLE", original);

        TestCase.assertEquals(original, title.get());
    }

    @Test
    public void test_title_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicReference<String> title = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TITLE", original);
        mViewModel.observeTitle(new TestLifeCycleOwner(), title::set);

        TestCase.assertEquals(original, title.get());
    }

    @Test
    public void test_title_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        String original = "a";
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> title = new AtomicReference<>();

        mSavedStateHandle.set("KEY_TITLE", original);
        mViewModel.observeTitle(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            title.set(s);
        });

        TestCase.assertEquals(original, title.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeDeviceType_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Integer> deviceType = new AtomicReference<>();

        mViewModel.observeDeviceType(new TestLifeCycleOwner(), deviceType::set);

        assertNull(deviceType.get());
    }

    @Test
    public void test_observeDeviceType_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<Integer> deviceType = new AtomicReference<>();

        mViewModel.observeDeviceType(new TestLifeCycleOwner(), deviceType::set);
        mSavedStateHandle.set("KEY_DEVICE_TYPE", original);

        TestCase.assertEquals(original, deviceType.get().intValue());
    }

    @Test
    public void test_observeDeviceType_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicReference<Integer> deviceType = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE", original);
        mViewModel.observeDeviceType(new TestLifeCycleOwner(), deviceType::set);

        TestCase.assertEquals(original, deviceType.get().intValue());
    }

    @Test
    public void test_observeDeviceType_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = 1;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<Integer> deviceType = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE", original);
        mViewModel.observeDeviceType(new TestLifeCycleOwner(), integer -> {
            count.incrementAndGet();
            deviceType.set(integer);
        });

        TestCase.assertEquals(original, deviceType.get().intValue());
        TestCase.assertEquals(1, count.get());
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

        int original = DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeName::set);
        mSavedStateHandle.set("KEY_DEVICE_TYPE", original);

        TestCase.assertEquals(mFakeDeviceSettingRepository.getDeviceTypeName(original), deviceTypeName.get());
    }

    @Test
    public void test_observeDeviceTypeName_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE", original);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), deviceTypeName::set);

        TestCase.assertEquals(mFakeDeviceSettingRepository.getDeviceTypeName(original), deviceTypeName.get());
    }

    @Test
    public void test_observeDeviceTypeName_00004() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        int original = DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
        AtomicInteger count = new AtomicInteger(0);
        AtomicReference<String> deviceTypeName = new AtomicReference<>();

        mSavedStateHandle.set("KEY_DEVICE_TYPE", original);
        mViewModel.observeDeviceTypeName(new TestLifeCycleOwner(), s -> {
            count.incrementAndGet();
            deviceTypeName.set(s);
        });

        TestCase.assertEquals(mFakeDeviceSettingRepository.getDeviceTypeName(original), deviceTypeName.get());
        TestCase.assertEquals(1, count.get());
    }

    @Test
    public void test_observeIsReady_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isReadyReference = new AtomicReference<>();

        mViewModel.observeIsReady(new TestLifeCycleOwner(), isReadyReference::set);

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> {
                }
                , throwable -> {
                });

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertTrue(isReadyReference.get());
    }

    @Test
    public void test_observeIsReady_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isReadyReference = new AtomicReference<>();

        mViewModel.observeIsReady(new TestLifeCycleOwner(), isReadyReference::set);

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> {
                }
                , throwable -> {
                });

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();
        mViewModel.clear();

        assertFalse(isReadyReference.get());
    }

    @Test
    public void test_isPeripheralReady_00001() {
        assertFalse(mViewModel.isPeripheralReady());
    }

    @Test
    public void test_isPeripheralReady_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> {
                }
                , throwable -> {
                });

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        assertTrue(mViewModel.isPeripheralReady());
    }

    @Test
    public void test_isPeripheralReady_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();

        mViewModel.observeSetup(new Intent()
                , () -> {
                }
                , throwable -> {
                });

        MockData mockData = new MockData();
        mockData.serviceDataList.add(new ServiceData(BLOOD_PRESSURE_SERVICE, BluetoothGattService.SERVICE_TYPE_PRIMARY, Collections.emptyList()));
        DeviceSetting original = new DeviceSetting(1, "a", DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mGson.toJson(mockData));
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingRepository.mLoadDeviceSettingByIdProcessor.onComplete();

        mViewModel.clear();

        assertFalse(mViewModel.isPeripheralReady());
    }

    @Test
    public void test_observeDeleteDeviceSetting_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicBoolean result = new AtomicBoolean(false);
        AtomicReference<DeviceSetting> deviceSettingReference = new AtomicReference<>();

        long original = 1;
        Intent intent = new Intent();
        intent.putExtra(KEY_DEVICE_ID, original);
        mFakeDeviceSettingRepository.mDeleteDeviceSettingConsumer = deviceSettingReference::set;
        mViewModel.observeDeleteDeviceSetting(intent, () -> result.set(true));

        assertEquals(original, deviceSettingReference.get().getId());
        assertTrue(result.get());
    }

}