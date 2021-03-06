package org.im97mori.ble.android.peripheral.ui.device.type;

import static junit.framework.TestCase.assertEquals;

import android.content.Context;
import android.os.Build;

import androidx.core.util.Pair;

import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
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

import dagger.hilt.android.qualifiers.ApplicationContext;
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

    private FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    @Inject
    DeviceSettingDataSource mDeviceSettingDataSource;

    @Inject
    @ApplicationContext
    Context mContext;

    private DeviceTypeListViewModel mViewModel;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mFakeDeviceSettingRepository = new FakeDeviceSettingRepository(mDeviceSettingDataSource, mContext);
        mViewModel = new DeviceTypeListViewModel(mFakeDeviceSettingRepository);
    }

    @After
    public void tearDown() {
        mViewModel = null;
    }

    @Test
    public void test_provideDeviceTypeList_00001() {
        List<Pair<Integer, String>> original = mFakeDeviceSettingRepository.provideDeviceTypeList();
        assertEquals(original, mViewModel.provideDeviceTypeList());
    }

    @Test
    public void test_provideDeviceTypeImageResMap_00001() {
        Map<Integer, Integer> original = mFakeDeviceSettingRepository.provideDeviceTypeImageResMap();
        assertEquals(original, mViewModel.provideDeviceTypeImageResMap());
    }

}