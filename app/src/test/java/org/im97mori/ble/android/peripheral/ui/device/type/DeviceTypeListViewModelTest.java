package org.im97mori.ble.android.peripheral.ui.device.type;

import static junit.framework.TestCase.assertEquals;

import android.os.Build;

import androidx.core.util.Pair;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceRepository;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceTypeListViewModelTest {
    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    FakeDeviceRepository mFakeDeviceRepository;

    private DeviceTypeListViewModel mViewModel;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mViewModel = new DeviceTypeListViewModel(mFakeDeviceRepository);
    }

    @After
    public void tearDown() {
        mViewModel = null;
    }

    @Test
    public void test_provideDeviceTypeList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceRepository.provideDeviceTypeList();
        assertEquals(original, mViewModel.provideDeviceTypeList());
    }

    @Test
    public void test_provideDeviceTypeImageResMap_00001() {
        Map<Integer, Integer> original = mFakeDeviceRepository.provideDeviceTypeImageResMap();
        assertEquals(original, mViewModel.provideDeviceTypeImageResMap());
    }

}