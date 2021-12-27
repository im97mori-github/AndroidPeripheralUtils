package org.im97mori.ble.android.peripheral.ui.main;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;

import android.os.Build;

import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.core.CompletableEmitter;
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
public class MainViewModelTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private MainViewModel mViewModel;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mViewModel = new MainViewModel(mFakeDeviceSettingRepository);
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
    }

    @Test
    public void test_observeLoadAllDeviceSetting_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        List<DeviceSetting> deviceSettingList = Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor = PublishProcessor.create();
        mViewModel.observeLoadAllDeviceSetting(devices -> {
            assertEquals(deviceSettingList, devices);
            result.set(true);
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor.onNext(deviceSettingList);
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor.onComplete();
        assertTrue(result.get());
    }

    @Test
    public void test_observeLoadAllDeviceSetting_00002() {
        AtomicBoolean result = new AtomicBoolean(false);
        List<DeviceSetting> deviceSettingList = Arrays.asList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)
                , new DeviceSetting(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor = PublishProcessor.create();
        mViewModel.observeLoadAllDeviceSetting(devices -> {
            assertEquals(deviceSettingList, devices);
            result.set(true);
        }, throwable -> {
        });
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor.onNext(deviceSettingList);
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor.onComplete();
        assertTrue(result.get());
    }

    @Test
    public void test_observeDeleteAllDeviceSetting_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());
        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingRepository.mDeleteAllDeviceSettingSubscribe = atomicReference::set;
        mViewModel.observeDeleteAllDeviceSetting(() -> result.set(true), throwable -> {
        });
        atomicReference.get().onComplete();

        assertTrue(result.get());
    }


    @Test
    public void test_dispose_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor = PublishProcessor.create();
        mViewModel.observeLoadAllDeviceSetting(devices -> result.set(true), throwable -> {
        });
        mViewModel.dispose();
        mFakeDeviceSettingRepository.mLoadAllDeviceSettingProcessor.onNext(Collections.emptyList());
        assertFalse(result.get());
    }

    @Test
    public void test_dispose_00002() {
        final AtomicBoolean result = new AtomicBoolean(false);
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());
        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingRepository.mDeleteAllDeviceSettingSubscribe = atomicReference::set;
        mViewModel.observeDeleteAllDeviceSetting(() -> result.set(true), throwable -> {
        });
        mViewModel.dispose();
        atomicReference.get().onComplete();

        assertFalse(result.get());
    }

}