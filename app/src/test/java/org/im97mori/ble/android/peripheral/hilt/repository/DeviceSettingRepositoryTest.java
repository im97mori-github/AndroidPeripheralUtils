package org.im97mori.ble.android.peripheral.hilt.repository;

import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static junit.framework.TestCase.assertNull;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;

import android.content.Context;
import android.os.Build;

import androidx.core.util.Pair;

import org.im97mori.ble.android.peripheral.Constants;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.datasource.FakeDeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.core.DateTimeUtils;
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
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.core.CompletableEmitter;
import io.reactivex.rxjava3.disposables.CompositeDisposable;
import io.reactivex.rxjava3.processors.PublishProcessor;

@SuppressWarnings("ConstantConditions")
@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceSettingRepositoryTest {

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Inject
    @ApplicationContext
    Context mContext;

    @Inject
    FakeDeviceSettingDataSource mFakeDeviceSettingDataSource;

    private DeviceSettingRepository mDeviceSettingRepository;

    private final CompositeDisposable mDisposable = new CompositeDisposable();

    @Before
    public void setUp() {
        mHiltRule.inject();
        mDeviceSettingRepository = new DeviceSettingRepository(mFakeDeviceSettingDataSource, mContext);
//        mDeviceSettingRepository.deleteAllDeviceSetting().blockingSubscribe();
    }

    @After
    public void tearDown() {
        mDisposable.clear();
    }

    @Test
    public void test_loadAllDeviceSetting_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        List<DeviceSetting> deviceSettingList = Collections.singletonList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mFakeDeviceSettingDataSource.mLoadAllDeviceSettingProcessor = PublishProcessor.create();
        mDisposable.add(mDeviceSettingRepository.loadAllDeviceSetting().subscribe(devices -> {
            assertEquals(deviceSettingList, devices);
            result.set(true);
        }, throwable -> {
        }));
        mFakeDeviceSettingDataSource.mLoadAllDeviceSettingProcessor.onNext(deviceSettingList);
        mFakeDeviceSettingDataSource.mLoadAllDeviceSettingProcessor.onComplete();
        assertTrue(result.get());
    }

    @Test
    public void test_loadAllDeviceSetting_00002() {
        AtomicBoolean result = new AtomicBoolean(false);
        List<DeviceSetting> deviceSettingList = Arrays.asList(new DeviceSetting(1, "a", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null)
                , new DeviceSetting(2, "b", Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, null));
        mFakeDeviceSettingDataSource.mLoadAllDeviceSettingProcessor = PublishProcessor.create();
        mDisposable.add(mDeviceSettingRepository.loadAllDeviceSetting().subscribe(devices -> {
            assertEquals(deviceSettingList, devices);
            result.set(true);
        }, throwable -> {
        }));
        mFakeDeviceSettingDataSource.mLoadAllDeviceSettingProcessor.onNext(deviceSettingList);
        mFakeDeviceSettingDataSource.mLoadAllDeviceSettingProcessor.onComplete();
        assertTrue(result.get());
    }

    @Test
    public void test_loadDeviceSettingById_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");
        mFakeDeviceSettingDataSource.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();
        mDisposable.add(mDeviceSettingRepository.loadDeviceSettingById(1).subscribe(deviceSetting -> {
            assertEquals(original, deviceSetting);
            result.set(true);
        }));
        mFakeDeviceSettingDataSource.mLoadDeviceSettingByIdProcessor.onNext(original);
        mFakeDeviceSettingDataSource.mLoadDeviceSettingByIdProcessor.onComplete();
        assertTrue(result.get());
    }

    @Test
    public void test_loadDeviceSettingById_00002() {
        AtomicBoolean result = new AtomicBoolean(false);
        mFakeDeviceSettingDataSource.mLoadDeviceSettingByIdProcessor = PublishProcessor.create();
        mDisposable.add(mDeviceSettingRepository.loadDeviceSettingById(1).subscribe(deviceSetting -> {
        }, throwable -> result.set(true)));
        mFakeDeviceSettingDataSource.mLoadDeviceSettingByIdProcessor.onError(new RuntimeException());
        mFakeDeviceSettingDataSource.mLoadDeviceSettingByIdProcessor.onComplete();
        assertTrue(result.get());
    }

    @Test
    public void test_insertDeviceSetting_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");

        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingDataSource.mInsertDeviceSettingSubscribe = atomicReference::set;
        mDisposable.add(mDeviceSettingRepository.insertDeviceSetting(original).subscribe(() -> result.set(true), throwable -> {
        }));
        atomicReference.get().onComplete();

        assertTrue(result.get());
    }

    @Test
    public void test_insertDeviceSetting_00002() {
        AtomicBoolean result = new AtomicBoolean(false);
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");

        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingDataSource.mInsertDeviceSettingSubscribe = atomicReference::set;
        mDisposable.add(mDeviceSettingRepository.insertDeviceSetting(original).subscribe(() -> {
        }, throwable -> result.set(true)));
        atomicReference.get().onError(new RuntimeException());

        assertTrue(result.get());
    }

    @Test
    public void test_deleteDeviceSetting_00001() {
        AtomicBoolean result = new AtomicBoolean(false);
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");

        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingDataSource.mDeleteDeviceSettingSubscribe = atomicReference::set;
        mDisposable.add(mDeviceSettingRepository.deleteDeviceSetting(original).subscribe(() -> result.set(true), throwable -> {
        }));
        atomicReference.get().onComplete();

        assertTrue(result.get());
    }

    @Test
    public void test_deleteDeviceSetting_00002() {
        AtomicBoolean result = new AtomicBoolean(false);
        DeviceSetting original = new DeviceSetting(1, "a", 2, "b");

        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingDataSource.mDeleteDeviceSettingSubscribe = atomicReference::set;
        mDisposable.add(mDeviceSettingRepository.deleteDeviceSetting(original).subscribe(() -> {
        }, throwable -> result.set(true)));
        atomicReference.get().onError(new RuntimeException());

        assertTrue(result.get());
    }

    @Test
    public void test_deleteAllDeviceSetting_00001() {
        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingDataSource.mDeleteAllDeviceSettingSubscribe = atomicReference::set;
        mDisposable.add(mDeviceSettingRepository.deleteAllDeviceSetting().subscribe(() -> result.set(true), throwable -> {
        }));
        atomicReference.get().onComplete();

        assertTrue(result.get());
    }

    @Test
    public void test_deleteAllDeviceSetting_00002() {
        AtomicBoolean result = new AtomicBoolean(false);

        AtomicReference<CompletableEmitter> atomicReference = new AtomicReference<>();
        mFakeDeviceSettingDataSource.mDeleteAllDeviceSettingSubscribe = atomicReference::set;
        mDisposable.add(mDeviceSettingRepository.deleteAllDeviceSetting().subscribe(() -> result.set(true), throwable -> {
        }));
        atomicReference.get().onComplete();

        assertTrue(result.get());
    }

    @Test
    public void test_getDeviceTypeImageResId_00001() {
        Integer resId = mDeviceSettingRepository.getDeviceTypeImageResId(DEVICE_TYPE_UNDEFINED);
        assertNull(resId);
    }

    @Test
    public void test_getDeviceTypeImageResId_00002() {
        Integer resId = mDeviceSettingRepository.getDeviceTypeImageResId(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        assertNotNull(resId);
        assertEquals(R.drawable.medical_ketsuatsukei_aneroid, resId.intValue());
    }

    @Test
    public void test_getDeviceTypeName_00001() {
        String text = mDeviceSettingRepository.getDeviceTypeName(DEVICE_TYPE_UNDEFINED);
        assertNull(text);
    }

    @Test
    public void test_getDeviceTypeName_00002() {
        String text = mDeviceSettingRepository.getDeviceTypeName(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        assertNotNull(text);
        assertEquals(mContext.getString(R.string.blood_pressure_profile), text);
    }

    @Test
    public void test_provideDeviceTypeImageResMap_00001() {
        Map<Integer, Integer> map = mDeviceSettingRepository.provideDeviceTypeImageResMap();
        Integer resId = map.get(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        assertNotNull(resId);
        assertEquals(R.drawable.medical_ketsuatsukei_aneroid, resId.intValue());
    }

    @Test
    public void test_provideDeviceTypeList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDeviceTypeList();
        Optional<Pair<Integer, String>> optional
                = list.stream().filter(integerStringPair -> integerStringPair.first == DEVICE_TYPE_BLOOD_PRESSURE_PROFILE
                && integerStringPair.second.equals(mContext.getString(R.string.blood_pressure_profile))).findFirst();
        assertTrue(optional.isPresent());
    }

    @Test
    public void test_provideDateTimeMonthList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(0);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_IS_NOT_KNOWN);
        assertEquals(pair.second, mContext.getString(R.string.month_is_not_known));
    }

    @Test
    public void test_provideDateTimeMonthList_00002() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(1);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_JANUARY);
        assertEquals(pair.second, mContext.getString(R.string.month_january));
    }

    @Test
    public void test_provideDateTimeMonthList_00003() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(2);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_FEBRUARY);
        assertEquals(pair.second, mContext.getString(R.string.month_february));
    }

    @Test
    public void test_provideDateTimeMonthList_00004() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(3);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_MARCH);
        assertEquals(pair.second, mContext.getString(R.string.month_march));
    }

    @Test
    public void test_provideDateTimeMonthList_00005() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(4);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_APRIL);
        assertEquals(pair.second, mContext.getString(R.string.month_april));
    }

    @Test
    public void test_provideDateTimeMonthList_00006() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(5);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_MAY);
        assertEquals(pair.second, mContext.getString(R.string.month_may));
    }

    @Test
    public void test_provideDateTimeMonthList_00007() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(6);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_JUNE);
        assertEquals(pair.second, mContext.getString(R.string.month_june));
    }

    @Test
    public void test_provideDateTimeMonthList_00008() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(7);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_JULY);
        assertEquals(pair.second, mContext.getString(R.string.month_july));
    }

    @Test
    public void test_provideDateTimeMonthList_00009() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(8);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_AUGUST);
        assertEquals(pair.second, mContext.getString(R.string.month_august));
    }

    @Test
    public void test_provideDateTimeMonthList_00010() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(9);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_SEPTEMBER);
        assertEquals(pair.second, mContext.getString(R.string.month_september));
    }

    @Test
    public void test_provideDateTimeMonthList_00011() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(10);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_OCTOBER);
        assertEquals(pair.second, mContext.getString(R.string.month_october));
    }

    @Test
    public void test_provideDateTimeMonthList_00012() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(11);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_NOVEMBER);
        assertEquals(pair.second, mContext.getString(R.string.month_november));
    }

    @Test
    public void test_provideDateTimeMonthList_00013() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeMonthList();
        Pair<Integer, String> pair = list.get(12);
        assertEquals(pair.first.intValue(), DateTimeUtils.MONTH_DECEMBER);
        assertEquals(pair.second, mContext.getString(R.string.month_december));
    }

    @Test
    public void test_provideDateTimeDayList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        Pair<Integer, String> pair = list.get(0);
        assertEquals(pair.first.intValue(), DateTimeUtils.DAY_OF_MONTH_IS_NOT_KNOWN);
        assertEquals(pair.second, mContext.getString(R.string.day_of_month_is_not_known));
    }

    @Test
    public void test_provideDateTimeDayList_00002() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 1;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00003() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 2;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00004() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 3;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00005() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 4;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00006() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 5;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00007() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 6;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00008() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 7;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00009() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 8;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00010() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 9;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00011() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 10;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00012() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 11;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00013() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 12;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00014() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 13;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00015() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 14;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00016() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 15;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00017() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 16;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00018() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 17;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00019() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 18;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00020() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 19;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00021() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 20;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00022() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 21;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00023() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 22;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00024() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 23;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00025() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 24;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00026() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 25;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00027() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 26;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00028() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 27;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00029() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 28;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00030() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 29;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00031() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 30;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeDayList_00032() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideDateTimeDayList();
        int index = 31;
        Pair<Integer, String> pair = list.get(index);
        assertEquals(pair.first.intValue(), index);
        assertEquals(pair.second, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00001() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 0;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00002() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 1;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00003() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 2;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00004() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 3;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00005() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 4;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00006() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 5;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00007() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 6;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00008() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 7;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00009() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 8;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00010() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 9;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00011() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 10;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00012() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 11;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00013() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 12;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00014() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 13;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00015() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 14;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00016() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 15;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00017() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 16;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00018() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 17;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00019() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 18;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00020() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 19;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00021() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 20;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00022() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 21;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00023() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 22;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeHoursList_00024() {
        List<String> list = mDeviceSettingRepository.provideDateTimeHoursList();
        int index = 23;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00001() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 0;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00002() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 1;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00003() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 2;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00004() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 3;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00005() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 4;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00006() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 5;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00007() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 6;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00008() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 7;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00009() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 8;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00010() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 9;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00011() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 10;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00012() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 11;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00013() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 12;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00014() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 13;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00015() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 14;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00016() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 15;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00017() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 16;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00018() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 17;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00019() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 18;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00020() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 19;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00021() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 20;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00022() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 21;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00023() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 22;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00024() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 23;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00025() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 24;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00026() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 25;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00027() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 26;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00028() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 27;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00029() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 28;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00030() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 29;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00031() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 30;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00032() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 31;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00033() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 32;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00034() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 33;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00035() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 34;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00036() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 35;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00037() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 36;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00038() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 37;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00039() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 38;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00040() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 39;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00041() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 40;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00042() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 41;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00043() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 42;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00044() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 43;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00045() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 44;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00046() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 45;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00047() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 46;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00048() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 47;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00049() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 48;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00050() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 49;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00051() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 50;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00052() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 51;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00053() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 52;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00054() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 53;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00055() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 54;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00056() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 55;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00057() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 56;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00058() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 57;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00059() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 58;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00060() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 59;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeMinutesList_00061() {
        List<String> list = mDeviceSettingRepository.provideDateTimeMinutesList();
        int index = 60;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00001() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 0;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00002() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 1;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00003() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 2;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00004() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 3;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00005() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 4;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00006() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 5;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00007() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 6;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00008() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 7;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00009() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 8;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00010() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 9;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00011() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 10;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00012() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 11;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00013() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 12;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00014() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 13;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00015() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 14;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00016() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 15;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00017() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 16;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00018() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 17;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00019() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 18;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00020() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 19;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00021() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 20;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00022() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 21;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00023() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 22;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00024() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 23;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00025() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 24;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00026() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 25;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00027() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 26;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00028() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 27;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00029() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 28;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00030() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 29;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00031() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 30;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00032() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 31;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00033() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 32;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00034() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 33;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00035() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 34;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00036() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 35;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00037() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 36;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00038() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 37;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00039() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 38;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00040() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 39;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00041() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 40;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00042() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 41;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00043() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 42;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00044() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 43;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00045() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 44;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00046() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 45;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00047() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 46;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00048() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 47;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00049() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 48;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00050() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 49;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00051() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 50;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00052() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 51;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00053() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 52;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00054() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 53;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00055() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 54;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00056() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 55;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00057() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 56;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00058() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 57;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00059() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 58;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00060() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 59;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideDateTimeSecondsList_00061() {
        List<String> list = mDeviceSettingRepository.provideDateTimeSecondsList();
        int index = 60;
        String text = list.get(index);
        assertEquals(text, String.valueOf(index));
    }

    @Test
    public void test_provideBodyMovementDetectionList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideBodyMovementDetectionList();
        Pair<Integer, String> pair = list.get(0);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_NO_BODY_MOVEMENT);
        assertEquals(pair.second, mContext.getString(R.string.no_body_movement));
    }

    @Test
    public void test_provideBodyMovementDetectionList_00002() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideBodyMovementDetectionList();
        Pair<Integer, String> pair = list.get(1);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT);
        assertEquals(pair.second, mContext.getString(R.string.body_movement_during_measurement));
    }

    @Test
    public void test_provideCuffFitDetectionList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideCuffFitDetectionList();
        Pair<Integer, String> pair = list.get(0);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_FITS_PROPERLY);
        assertEquals(pair.second, mContext.getString(R.string.cuff_fits_properly));
    }

    @Test
    public void test_provideCuffFitDetectionList_00002() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideCuffFitDetectionList();
        Pair<Integer, String> pair = list.get(1);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE);
        assertEquals(pair.second, mContext.getString(R.string.cuff_too_loose));
    }

    @Test
    public void test_provideIrregularPulseDetectionList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideIrregularPulseDetectionList();
        Pair<Integer, String> pair = list.get(0);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_NO_IRREGULAR_PULSE_DETECTED);
        assertEquals(pair.second, mContext.getString(R.string.no_irregular_pulse_detected));
    }

    @Test
    public void test_provideIrregularPulseDetectionList_00002() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideIrregularPulseDetectionList();
        Pair<Integer, String> pair = list.get(1);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED);
        assertEquals(pair.second, mContext.getString(R.string.irregular_pulse_detected));
    }

    @Test
    public void test_providePulseRateRangeDetectionList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.providePulseRateRangeDetectionList();
        Pair<Integer, String> pair = list.get(0);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_WITHIN_THE_RANGE);
        assertEquals(pair.second, mContext.getString(R.string.pulse_rate_is_within_the_range));
    }

    @Test
    public void test_providePulseRateRangeDetectionList_00002() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.providePulseRateRangeDetectionList();
        Pair<Integer, String> pair = list.get(1);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_EXCEEDS_UPPER_LIMIT);
        assertEquals(pair.second, mContext.getString(R.string.pulse_rate_exceeds_upper_limit));
    }

    @Test
    public void test_providePulseRateRangeDetectionList_00003() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.providePulseRateRangeDetectionList();
        Pair<Integer, String> pair = list.get(2);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT);
        assertEquals(pair.second, mContext.getString(R.string.pulse_rate_is_less_than_lower_limit));
    }

    @Test
    public void test_provideMeasurementPositionDetectionList_00001() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideMeasurementPositionDetectionList();
        Pair<Integer, String> pair = list.get(0);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_PROPER_MEASUREMENT_POSITION);
        assertEquals(pair.second, mContext.getString(R.string.proper_measurement_position));
    }

    @Test
    public void test_provideMeasurementPositionDetectionList_00002() {
        List<Pair<Integer, String>> list = mDeviceSettingRepository.provideMeasurementPositionDetectionList();
        Pair<Integer, String> pair = list.get(1);
        assertEquals(pair.first.intValue(), BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION);
        assertEquals(pair.second, mContext.getString(R.string.improper_measurement_position));
    }

    @Test
    public void test_getDeviceSettingNameErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getDeviceSettingNameErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getDeviceSettingNameErrorString_00002() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getDeviceSettingNameErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getSystolicErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getSystolicErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getSystolicErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getSystolicErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getSystolicErrorString_00003() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getSystolicErrorString(text);
        assertEquals(mContext.getString(R.string.wrong_format), result);
    }

    @Test
    public void test_getSystolicErrorString_00004() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getSystolicErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getDiastolicErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getDiastolicErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getDiastolicErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getDiastolicErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getDiastolicErrorString_00003() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getDiastolicErrorString(text);
        assertEquals(mContext.getString(R.string.wrong_format), result);
    }

    @Test
    public void test_getDiastolicErrorString_00004() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getDiastolicErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getMeanArterialPressureErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getMeanArterialPressureErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getMeanArterialPressureErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getMeanArterialPressureErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getMeanArterialPressureErrorString_00003() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getMeanArterialPressureErrorString(text);
        assertEquals(mContext.getString(R.string.wrong_format), result);
    }

    @Test
    public void test_getMeanArterialPressureErrorString_00004() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getMeanArterialPressureErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getPulseRateErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getPulseRateErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getPulseRateErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getPulseRateErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getPulseRateErrorString_00003() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getPulseRateErrorString(text);
        assertEquals(mContext.getString(R.string.wrong_format), result);
    }

    @Test
    public void test_getPulseRateErrorString_00004() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getPulseRateErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getUserIdErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getUserIdErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getUserIdErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getUserIdErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getUserIdErrorString_00003() {
        CharSequence text = "-1";
        String result = mDeviceSettingRepository.getUserIdErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getUserIdErrorString_00004() {
        CharSequence text = "256";
        String result = mDeviceSettingRepository.getUserIdErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getUserIdErrorString_00005() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getUserIdErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getNotificationCountErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getNotificationCountErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getNotificationCountErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getNotificationCountErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getNotificationCountErrorString_00003() {
        CharSequence text = "-2";
        String result = mDeviceSettingRepository.getNotificationCountErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getNotificationCountErrorString_00004() {
        CharSequence text = "0";
        String result = mDeviceSettingRepository.getNotificationCountErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getNotificationCountErrorString_00005() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getNotificationCountErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getIndicationCountErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getIndicationCountErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getIndicationCountErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getIndicationCountErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getIndicationCountErrorString_00003() {
        CharSequence text = "-2";
        String result = mDeviceSettingRepository.getIndicationCountErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getIndicationCountErrorString_00004() {
        CharSequence text = "0";
        String result = mDeviceSettingRepository.getIndicationCountErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getIndicationCountErrorString_00005() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getIndicationCountErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getDateTimeYearErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getDateTimeYearErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getDateTimeYearErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getDateTimeYearErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getDateTimeYearErrorString_00003() {
        CharSequence text = "1581";
        String result = mDeviceSettingRepository.getDateTimeYearErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getDateTimeYearErrorString_00004() {
        CharSequence text = "10000";
        String result = mDeviceSettingRepository.getDateTimeYearErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getDateTimeYearErrorString_00005() {
        CharSequence text = "1582";
        String result = mDeviceSettingRepository.getDateTimeYearErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getResponseDelayErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getResponseDelayErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getResponseDelayErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getResponseDelayErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getResponseDelayErrorString_00003() {
        CharSequence text = "-1";
        String result = mDeviceSettingRepository.getResponseDelayErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getResponseDelayErrorString_00004() {
        CharSequence text = "0";
        String result = mDeviceSettingRepository.getResponseDelayErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getResponseCodeErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getResponseCodeErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getResponseCodeErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getResponseCodeErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getResponseCodeErrorString_00003() {
        CharSequence text = "0";
        String result = mDeviceSettingRepository.getResponseCodeErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getResponseCodeErrorString_00004() {
        CharSequence text = "100";
        String result = mDeviceSettingRepository.getResponseCodeErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getResponseCodeErrorString_00005() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getResponseCodeErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getCurrentCuffPressureErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getCurrentCuffPressureErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getCurrentCuffPressureErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getCurrentCuffPressureErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getCurrentCuffPressureErrorString_00003() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getCurrentCuffPressureErrorString(text);
        assertEquals(mContext.getString(R.string.wrong_format), result);
    }

    @Test
    public void test_getCurrentCuffPressureErrorString_00004() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getCurrentCuffPressureErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getManufacturerNameStringErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getManufacturerNameStringErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getManufacturerNameStringErrorString_00002() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getManufacturerNameStringErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getModelNumberErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getModelNumberStringErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getModelNumberErrorString_00002() {
        CharSequence text = "a";
        String result = mDeviceSettingRepository.getModelNumberStringErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getManufacturerIdentifierErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getManufacturerIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getManufacturerIdentifierErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getManufacturerIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getManufacturerIdentifierErrorString_00003() {
        CharSequence text = "-1";
        String result = mDeviceSettingRepository.getManufacturerIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getManufacturerIdentifierErrorString_00004() {
        CharSequence text = "1099511627776";
        String result = mDeviceSettingRepository.getManufacturerIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getManufacturerIdentifierErrorString_00005() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getManufacturerIdentifierErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getOrganizationallyUniqueIdentifierErrorString_00001() {
        CharSequence text = null;
        String result = mDeviceSettingRepository.getOrganizationallyUniqueIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getOrganizationallyUniqueIdentifierErrorString_00002() {
        CharSequence text = "";
        String result = mDeviceSettingRepository.getOrganizationallyUniqueIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.no_value), result);
    }

    @Test
    public void test_getOrganizationallyUniqueIdentifierErrorString_00003() {
        CharSequence text = "-1";
        String result = mDeviceSettingRepository.getOrganizationallyUniqueIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getOrganizationallyUniqueIdentifierErrorString_00004() {
        CharSequence text = "16777216";
        String result = mDeviceSettingRepository.getOrganizationallyUniqueIdentifierErrorString(text);
        assertEquals(mContext.getString(R.string.out_of_range), result);
    }

    @Test
    public void test_getOrganizationallyUniqueIdentifierErrorString_00005() {
        CharSequence text = "1";
        String result = mDeviceSettingRepository.getOrganizationallyUniqueIdentifierErrorString(text);
        assertNull(result);
    }

    @Test
    public void test_getUnitString_00001() {
        boolean isMmhg = true;
        String result = mDeviceSettingRepository.getUnitString(isMmhg);
        assertEquals(mContext.getString(R.string.mmhg), result);
    }

    @Test
    public void test_getUnitString_00002() {
        boolean isMmhg = false;
        String result = mDeviceSettingRepository.getUnitString(isMmhg);
        assertEquals(mContext.getString(R.string.kpa), result);
    }

    @Test
    public void test_getHexString_00001() {
        int decimal = 10;
        int length = 2;
        String result = mDeviceSettingRepository.getHexString(decimal, length);
        assertEquals("0x0a", result);
    }

    @Test
    public void test_getHexString_00002() {
        int decimal = 16;
        int length = 4;
        String result = mDeviceSettingRepository.getHexString(decimal, length);
        assertEquals("0x0010", result);
    }

    @Test
    public void test_getDateTimeString_00001() {
        int year = 1;
        int month = 2;
        int day = 3;
        int hours = 4;
        int minutes = 5;
        int seconds = 6;
        String result = mDeviceSettingRepository.getDateTimeString(year, month, day, hours, minutes, seconds);
        assertEquals(String.format(Locale.US
                , "%1$04d-%2$02d-%3$02d %4$02d:%5$02d:%6$02d"
                , year
                , month
                , day
                , hours
                , minutes
                , seconds), result);
    }

    @Test
    public void test_getIndicationsDisabledString_00001() {
        String result = mDeviceSettingRepository.getIndicationsDisabledString();
        assertEquals(mContext.getString(R.string.indication_disabled), result);
    }

    @Test
    public void test_getIndicationsEnabledString_00001() {
        String result = mDeviceSettingRepository.getIndicationsEnabledString();
        assertEquals(mContext.getString(R.string.indication_enabled), result);
    }

    @Test
    public void test_getNotificationsDisabledString_00001() {
        String result = mDeviceSettingRepository.getNotificationsDisabledString();
        assertEquals(mContext.getString(R.string.notification_disabled), result);
    }

    @Test
    public void test_getNotificationsEnabledString_00001() {
        String result = mDeviceSettingRepository.getNotificationsEnabledString();
        assertEquals(mContext.getString(R.string.notification_enabled), result);
    }

    @Test
    public void test_getIndicationsString_00001() {
        boolean enabled = true;
        String result = mDeviceSettingRepository.getIndicationsString(enabled);
        assertEquals(mContext.getString(R.string.indication_enabled), result);
    }

    @Test
    public void test_getIndicationsString_00002() {
        boolean enabled = false;
        String result = mDeviceSettingRepository.getIndicationsString(enabled);
        assertEquals(mContext.getString(R.string.indication_disabled), result);
    }

    @Test
    public void test_getNotificationsString_00001() {
        boolean enabled = true;
        String result = mDeviceSettingRepository.getNotificationsString(enabled);
        assertEquals(mContext.getString(R.string.notification_enabled), result);
    }

    @Test
    public void test_getNotificationsString_00002() {
        boolean enabled = false;
        String result = mDeviceSettingRepository.getNotificationsString(enabled);
        assertEquals(mContext.getString(R.string.notification_disabled), result);
    }

}