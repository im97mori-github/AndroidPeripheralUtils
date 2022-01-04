package org.im97mori.ble.android.peripheral.ui.device;

import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.utils.Utils.stackLog;
import static org.im97mori.ble.constants.ServiceUUID.BLOOD_PRESSURE_SERVICE;

import android.bluetooth.BluetoothGattService;
import android.content.Context;
import android.content.Intent;

import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.MockData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeBluetoothSettingRepository;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.test.TestLifeCycleOwner;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;

import java.util.Collections;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.processors.PublishProcessor;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltAndroidTest
public class PeripheralViewModelAndroidTest {

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private FakeBluetoothSettingRepository mFakeBluetoothSettingRepository;

    private PeripheralViewModel mViewModel;

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
        mFakeBluetoothSettingRepository = new FakeBluetoothSettingRepository(mContext);
        mViewModel = new PeripheralViewModel(mSavedStateHandle
                , mFakeDeviceSettingRepository
                , mFakeBluetoothSettingRepository
                , mGson);
        enableBluetooth();
    }

    @After
    public void tearDown() {
        mViewModel.quit();
        mViewModel.dispose();
        mViewModel = null;
        mSavedStateHandle = null;
        mFakeDeviceSettingRepository = null;
        mFakeBluetoothSettingRepository = null;
    }

    private void enableBluetooth() {
        if (!mFakeBluetoothSettingRepository.isBluetoothEnabled()) {
            CountDownLatch countDownLatch = new CountDownLatch(1);
            mFakeBluetoothSettingRepository.addBluetoothStatusConsumer(aBoolean -> {
                if (aBoolean) {
                    countDownLatch.countDown();
                }
            });
            mFakeBluetoothSettingRepository.bluetoothEnable();

            boolean done = false;
            do {
                try {
                    done = countDownLatch.await(1, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    stackLog(e);
                }
            } while (!done);
        }
    }

    @Test
    public void test_observeIsStarted_00001() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isStartedReference = new AtomicReference<>();

        mViewModel.observeIsStarted(new TestLifeCycleOwner(), isStartedReference::set);

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

        assertFalse(isStartedReference.get());
    }

    @Test
    public void test_observeIsStarted_00002() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isStartedReference = new AtomicReference<>();

        mViewModel.observeIsStarted(new TestLifeCycleOwner(), isStartedReference::set);

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

        mViewModel.start();
        assertTrue(isStartedReference.get());
    }

    @Test
    public void test_observeIsStarted_00003() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        AtomicReference<Boolean> isStartedReference = new AtomicReference<>();

        mViewModel.observeIsStarted(new TestLifeCycleOwner(), isStartedReference::set);

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

        mViewModel.start();
        mViewModel.quit();
        assertFalse(isStartedReference.get());
    }

    @Test
    public void test_isPeripheralStarted_00001() {
        assertFalse(mViewModel.isPeripheralStarted());
    }

    @Test
    public void test_isPeripheralStarted_00002() {
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

        assertFalse(mViewModel.isPeripheralStarted());
    }

    @Test
    public void test_isPeripheralStarted_00003() {
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

        mViewModel.start();
        assertTrue(mViewModel.isPeripheralStarted());
    }

    @Test
    public void test_isPeripheralStarted_00004() {
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

        mViewModel.start();
        mViewModel.quit();
        assertFalse(mViewModel.isPeripheralStarted());
    }

    @Test
    public void test_isPeripheralStarted_00005() {
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

        mViewModel.start();
        mViewModel.clear();
        assertFalse(mViewModel.isPeripheralStarted());
    }

    @Test
    public void test_start_00001() {
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

        mViewModel.start();
        assertTrue(mViewModel.isPeripheralStarted());
    }

    @Test
    public void test_quit_00001() {
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

        mViewModel.start();
        mViewModel.quit();
        assertFalse(mViewModel.isPeripheralStarted());
    }

    @Test
    public void test_clear_00001() {
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

        mViewModel.start();
        mViewModel.clear();
        assertFalse(mViewModel.isPeripheralStarted());
        assertFalse(mViewModel.isPeripheralReady());
    }

}