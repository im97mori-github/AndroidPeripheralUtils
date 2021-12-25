package org.im97mori.ble.android.peripheral.ui.main;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;

import android.os.Build;

import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.room.Device;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.inject.Inject;

import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.disposables.CompositeDisposable;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class, sdk = Build.VERSION_CODES.LOLLIPOP)
public class MainViewModelTest {

    @Rule
    public HiltAndroidRule hiltRule = new HiltAndroidRule(this);

    @Inject
    FakeDeviceRepository mFakeDeviceRepository;

    private MainViewModel mMainViewModel;

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    @BeforeClass
    public static void setUpClass() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());
    }

    @Before
    public void setUp() {
        hiltRule.inject();
        mMainViewModel = new MainViewModel(mFakeDeviceRepository);
    }

    @After
    public void tearDown() {
        mMainViewModel = null;
        mDisposable.dispose();
        mDisposable.clear();
    }

    @Test
    public void test_getDeviceList_00001() {
        List<Device> deviceList = Collections.singletonList(new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mDisposable.add(mMainViewModel.getDeviceList().subscribe(devices -> assertEquals(deviceList, devices)));
        mFakeDeviceRepository.getLoadDevicesProcessor.onNext(deviceList);
    }

    @Test
    public void test_getDeviceList_00002() {
        List<Device> deviceList = Arrays.asList(new Device(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)
                , new Device(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mDisposable.add(mMainViewModel.getDeviceList().subscribe(devices -> assertEquals(deviceList, devices)));
        mFakeDeviceRepository.getLoadDevicesProcessor.onNext(deviceList);
    }

    @Test
    public void test_deleteAllDevices_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        mFakeDeviceRepository.setDeleteAllDevicesConsumer(result::set);
        mDisposable.add(mMainViewModel.deleteAllDevices().subscribe());
        assertTrue(result.get());
    }


    @Test
    public void test_provideDeviceTypeImageResMap_00001() {
        Map<Integer, Integer> original = mFakeDeviceRepository.provideDeviceTypeImageResMap();
        assertEquals(original, mMainViewModel.provideDeviceTypeImageResMap());
    }

}