package org.im97mori.ble.android.peripheral.ui.main;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertFalse;
import static junit.framework.TestCase.assertTrue;

import android.os.Build;

import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceRepository;
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
import io.reactivex.rxjava3.observers.TestObserver;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.processors.PublishProcessor;
import io.reactivex.rxjava3.schedulers.Schedulers;
import io.reactivex.rxjava3.subscribers.TestSubscriber;

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
    FakeDeviceRepository mFakeDeviceRepository;

    private MainViewModel mViewModel;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mViewModel = new MainViewModel(mFakeDeviceRepository);
    }

    @After
    public void tearDown() {
        mViewModel.dispose();
        mViewModel = null;
    }

    @Test
    public void test_observeAllDeviceSettings_00001() {
        List<DeviceSetting> deviceSettingList = Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mFakeDeviceRepository.mLoadAllDeviceSettingsProcessor = PublishProcessor.create();
        mViewModel.observeAllDeviceSettings(devices -> assertEquals(deviceSettingList, devices), throwable -> {
        });
        mFakeDeviceRepository.mLoadAllDeviceSettingsProcessor.onNext(deviceSettingList);
    }

    @Test
    public void test_observeAllDeviceSettings_00002() {
        List<DeviceSetting> deviceSettingList = Arrays.asList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)
                , new DeviceSetting(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mFakeDeviceRepository.mLoadAllDeviceSettingsProcessor = PublishProcessor.create();
        mViewModel.observeAllDeviceSettings(devices -> assertEquals(deviceSettingList, devices), throwable -> {
        });
        mFakeDeviceRepository.mLoadAllDeviceSettingsProcessor.onNext(deviceSettingList);
    }

    @Test
    public void test_observeDeleteAllDeviceSettings_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());
        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceRepository.mDeleteAllDeviceSettingsSubscribe = atomicReference::set;
        mViewModel.observeDeleteAllDeviceSettings(() -> result.set(true), throwable -> {
        });
        atomicReference.get().onComplete();

        assertTrue(result.get());
    }


    @Test
    public void test_dispose_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        mFakeDeviceRepository.mLoadAllDeviceSettingsProcessor = PublishProcessor.create();
        mViewModel.observeAllDeviceSettings(devices -> result.set(true), throwable -> {
        });
        mViewModel.dispose();
        mFakeDeviceRepository.mLoadAllDeviceSettingsProcessor.onNext(Collections.emptyList());
        assertFalse(result.get());
    }

    @Test
    public void test_dispose_00002() {
        final AtomicBoolean result = new AtomicBoolean(false);
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());
        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceRepository.mDeleteAllDeviceSettingsSubscribe = atomicReference::set;
        mViewModel.observeDeleteAllDeviceSettings(() -> result.set(true), throwable -> {
        });
        mViewModel.dispose();
        atomicReference.get().onComplete();

        assertFalse(result.get());
    }

}