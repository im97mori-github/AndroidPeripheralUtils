package org.im97mori.ble.android.peripheral.room;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;

import android.content.Context;
import android.os.Build;

import androidx.room.Room;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

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
public class DeviceSettingDaoTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    private AppDatabase mAppDatabase;

    private DeviceSettingDao mDeviceSettingDao;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mAppDatabase = Room.inMemoryDatabaseBuilder(mContext, AppDatabase.class)
                .allowMainThreadQueries()
                .build();
        mDeviceSettingDao = mAppDatabase.getDeviceSettingDao();
    }

    @After
    public void closeDb() {
        mAppDatabase.close();
    }

    @Test
    public void test_loadAllDeviceSetting_00001() {
        List<DeviceSetting> deviceSettingList = mDeviceSettingDao.loadAllDeviceSetting().blockingFirst();
        assertTrue(deviceSettingList.isEmpty());
    }

    @Test
    public void test_loadAllDeviceSetting_00002() {
        DeviceSetting original = new DeviceSetting("a", 1);
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();

        List<DeviceSetting> deviceSettingList = mDeviceSettingDao.loadAllDeviceSetting().blockingFirst();
        assertEquals(1, deviceSettingList.size());
        DeviceSetting deviceSetting = deviceSettingList.get(0);
        assertTrue(deviceSetting.getId() > 0);
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertNull(deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_loadAllDeviceSetting_00003() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();

        List<DeviceSetting> deviceSettingList = mDeviceSettingDao.loadAllDeviceSetting().blockingFirst();
        assertEquals(1, deviceSettingList.size());
        DeviceSetting deviceSetting = deviceSettingList.get(0);
        assertEquals(original.getId(), deviceSetting.getId());
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertNull(original.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_loadDeviceSettingById_00001() {
        final AtomicBoolean result = new AtomicBoolean(false);
        mDeviceSettingDao.loadDeviceSettingById(1).blockingSubscribe(deviceSetting -> {
        }, throwable -> result.set(true));
        assertTrue(result.get());
    }

    @Test
    public void test_loadDeviceSettingById_00002() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDao.loadDeviceSettingById(original.getId()).blockingGet();
        assertEquals(original.getId(), deviceSetting.getId());
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(original.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_loadDeviceSettingById_00003() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();

        final AtomicBoolean result = new AtomicBoolean(false);
        mDeviceSettingDao.loadDeviceSettingById(original.getId() + 1).blockingSubscribe(deviceSetting -> {
        }, throwable -> result.set(true));
        assertTrue(result.get());
    }

    @Test
    public void test_insertDeviceSetting_00001() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDao.loadDeviceSettingById(original.getId()).blockingGet();
        assertEquals(original.getId(), deviceSetting.getId());
        assertEquals(original.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(original.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_insertDeviceSetting_00002() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();
        DeviceSetting replaced = new DeviceSetting(1, "aa", 22, "bb");
        mDeviceSettingDao.insertDeviceSetting(replaced).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDao.loadDeviceSettingById(original.getId()).blockingGet();
        assertEquals(replaced.getId(), deviceSetting.getId());
        assertEquals(replaced.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(replaced.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(replaced.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_deleteDeviceSetting_00001() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();

        mDeviceSettingDao.deleteDeviceSetting(original).blockingSubscribe();
        List<DeviceSetting> deviceSettingList = mDeviceSettingDao.loadAllDeviceSetting().blockingFirst();
        assertTrue(deviceSettingList.isEmpty());
    }

    @Test
    public void test_deleteDeviceSetting_00002() {
        DeviceSetting original1 = new DeviceSetting(1, "a", 2, "b");
        DeviceSetting original2 = new DeviceSetting(2, "aa", 22, "bb");
        mDeviceSettingDao.insertDeviceSetting(original1).blockingSubscribe();
        mDeviceSettingDao.insertDeviceSetting(original2).blockingSubscribe();

        mDeviceSettingDao.deleteDeviceSetting(original1).blockingSubscribe();

        DeviceSetting deviceSetting = mDeviceSettingDao.loadDeviceSettingById(original2.getId()).blockingGet();
        assertEquals(original2.getId(), deviceSetting.getId());
        assertEquals(original2.getDeviceSettingName(), deviceSetting.getDeviceSettingName());
        assertEquals(original2.getDeviceType(), deviceSetting.getDeviceType());
        assertEquals(original2.getDeviceSettingData(), deviceSetting.getDeviceSettingData());
    }

    @Test
    public void test_deleteAllDeviceSetting_00001() {
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mDeviceSettingDao.insertDeviceSetting(original).blockingSubscribe();

        mDeviceSettingDao.deleteAllDeviceSetting().blockingSubscribe();
        List<DeviceSetting> deviceSettingList = mDeviceSettingDao.loadAllDeviceSetting().blockingFirst();
        assertTrue(deviceSettingList.isEmpty());
    }

    @Test
    public void test_deleteAllDeviceSetting_00002() {
        DeviceSetting original1 = new DeviceSetting(1, "a", 2, "b");
        DeviceSetting original2 = new DeviceSetting(2, "aa", 22, "bb");
        mDeviceSettingDao.insertDeviceSetting(original1).blockingSubscribe();
        mDeviceSettingDao.insertDeviceSetting(original2).blockingSubscribe();

        mDeviceSettingDao.deleteAllDeviceSetting().blockingSubscribe();
        List<DeviceSetting> deviceSettingList = mDeviceSettingDao.loadAllDeviceSetting().blockingFirst();
        assertTrue(deviceSettingList.isEmpty());
    }

}