package org.im97mori.ble.android.peripheral.hilt.repository;

import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;

import android.content.Context;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.datasource.DeviceSettingDataSource;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;
import org.im97mori.ble.android.peripheral.utils.IntegerStringPair;
import org.im97mori.ble.characteristic.core.BloodPressureMeasurementUtils;
import org.im97mori.ble.characteristic.core.DateTimeUtils;
import org.im97mori.ble.characteristic.core.IEEE_11073_20601_SFLOAT;
import org.im97mori.ble.constants.ErrorCode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.core.Single;

public class DeviceSettingRepository {

    private final DeviceSettingDataSource mDeviceSettingDataSource;
    private final Context mApplicationContext;

    private Map<Integer, Integer> mDeviceTypeImageResIdMap;
    private Map<Integer, String> mDeviceTypeNameMap;
    private List<Pair<Integer, String>> mDeviceTypeNameList;

    private List<Pair<Integer, String>> mDateTimeMonthList;
    private List<Pair<Integer, String>> mDateTimeDayList;
    private List<String> mDateTimeHoursList;
    private List<String> mDateTimeMinutesList;
    private List<String> mDateTimeSecondsList;
    private List<Pair<Integer, String>> mBodyMovementDetectionList;
    private List<Pair<Integer, String>> mCuffFitDetectionList;
    private List<Pair<Integer, String>> mIrregularPulseDetectionList;
    private List<Pair<Integer, String>> mPulseRateRangeDetectionList;
    private List<Pair<Integer, String>> mMeasurementPositionDetectionList;

    @Inject
    public DeviceSettingRepository(@NonNull DeviceSettingDataSource deviceSettingDataSource
            , @NonNull @ApplicationContext Context context) {
        mDeviceSettingDataSource = deviceSettingDataSource;
        mApplicationContext = context.getApplicationContext();
    }

    private synchronized void initDataType() {
        if (mDeviceTypeImageResIdMap == null) {
            Map<Integer, Integer> imageMap = Collections.synchronizedMap(new HashMap<>());
            imageMap.put(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, R.drawable.medical_ketsuatsukei_aneroid);
            mDeviceTypeImageResIdMap = Collections.unmodifiableMap(imageMap);

            Map<Integer, String> nameMap = Collections.synchronizedMap(new HashMap<>());
            nameMap.put(DEVICE_TYPE_BLOOD_PRESSURE_PROFILE, mApplicationContext.getString(R.string.blood_pressure_profile));
            mDeviceTypeNameMap = Collections.unmodifiableMap(nameMap);

            List<Pair<Integer, String>> list = nameMap.entrySet()
                    .stream()
                    .sorted((o1, o2) -> o2.getValue().compareTo(o1.getValue()))
                    .map(entry -> Pair.create(entry.getKey(), entry.getValue()))
                    .collect(Collectors.toList());
            mDeviceTypeNameList = Collections.unmodifiableList(Collections.synchronizedList(list));
        }
    }

    private synchronized void initDateTimeMonthList() {
        if (mDateTimeMonthList == null) {
            List<IntegerStringPair> list = Collections.synchronizedList(new ArrayList<>());
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_IS_NOT_KNOWN, mApplicationContext.getString(R.string.month_is_not_known)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_JANUARY, mApplicationContext.getString(R.string.month_january)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_FEBRUARY, mApplicationContext.getString(R.string.month_february)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_MARCH, mApplicationContext.getString(R.string.month_march)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_APRIL, mApplicationContext.getString(R.string.month_april)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_MAY, mApplicationContext.getString(R.string.month_may)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_JUNE, mApplicationContext.getString(R.string.month_june)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_JULY, mApplicationContext.getString(R.string.month_july)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_AUGUST, mApplicationContext.getString(R.string.month_august)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_SEPTEMBER, mApplicationContext.getString(R.string.month_september)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_OCTOBER, mApplicationContext.getString(R.string.month_october)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_NOVEMBER, mApplicationContext.getString(R.string.month_november)));
            list.add(new IntegerStringPair(DateTimeUtils.MONTH_DECEMBER, mApplicationContext.getString(R.string.month_december)));
            mDateTimeMonthList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initDateTimeDayList() {
        if (mDateTimeDayList == null) {
            List<IntegerStringPair> list = Collections.synchronizedList(new ArrayList<>());
            list.add(new IntegerStringPair(DateTimeUtils.DAY_OF_MONTH_IS_NOT_KNOWN, mApplicationContext.getString(R.string.day_of_month_is_not_known)));
            for (int i = 1; i < 32; i++) {
                list.add(new IntegerStringPair(i, Integer.toString(i)));
            }
            mDateTimeDayList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initDateTimeHoursList() {
        if (mDateTimeHoursList == null) {
            List<String> list = Collections.synchronizedList(new ArrayList<>());
            for (int i = 0; i < 24; i++) {
                list.add(Integer.toString(i));
            }
            mDateTimeHoursList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initDateTimeMinutesList() {
        if (mDateTimeMinutesList == null) {
            List<String> list = Collections.synchronizedList(new ArrayList<>());
            for (int i = 0; i < 61; i++) {
                list.add(Integer.toString(i));
            }
            mDateTimeMinutesList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initDateTimeSecondsList() {
        if (mDateTimeSecondsList == null) {
            List<String> list = Collections.synchronizedList(new ArrayList<>());
            for (int i = 0; i < 61; i++) {
                list.add(Integer.toString(i));
            }
            mDateTimeSecondsList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initBodyMovementDetectionList() {
        if (mBodyMovementDetectionList == null) {
            List<IntegerStringPair> list = Collections.synchronizedList(new ArrayList<>());
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_NO_BODY_MOVEMENT
                    , mApplicationContext.getString(R.string.no_body_movement)));
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_BODY_MOVEMENT_DETECTION_BODY_MOVEMENT_DURING_MEASUREMENT
                    , mApplicationContext.getString(R.string.body_movement_during_measurement)));
            mBodyMovementDetectionList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initCuffFitDetectionList() {
        if (mCuffFitDetectionList == null) {
            List<IntegerStringPair> list = Collections.synchronizedList(new ArrayList<>());
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_FITS_PROPERLY
                    , mApplicationContext.getString(R.string.cuff_fits_properly)));
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_CUFF_FIT_DETECTION_CUFF_TOO_LOOSE
                    , mApplicationContext.getString(R.string.cuff_too_loose)));
            mCuffFitDetectionList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initIrregularPulseDetectionList() {
        if (mIrregularPulseDetectionList == null) {
            List<IntegerStringPair> list = Collections.synchronizedList(new ArrayList<>());
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_NO_IRREGULAR_PULSE_DETECTED
                    , mApplicationContext.getString(R.string.no_irregular_pulse_detected)));
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_IRREGULAR_PULSE_DETECTION_IRREGULAR_PULSE_DETECTED
                    , mApplicationContext.getString(R.string.irregular_pulse_detected)));
            mIrregularPulseDetectionList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initPulseRateRangeDetectionList() {
        if (mPulseRateRangeDetectionList == null) {
            List<IntegerStringPair> list = Collections.synchronizedList(new ArrayList<>());
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_WITHIN_THE_RANGE
                    , mApplicationContext.getString(R.string.pulse_rate_is_within_the_range)));
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_EXCEEDS_UPPER_LIMIT
                    , mApplicationContext.getString(R.string.pulse_rate_exceeds_upper_limit)));
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_PULSE_RATE_RANGE_DETECTION_PULSE_RATE_IS_LESS_THAN_LOWER_LIMIT
                    , mApplicationContext.getString(R.string.pulse_rate_is_less_than_lower_limit)));
            mPulseRateRangeDetectionList = Collections.unmodifiableList(list);
        }
    }

    private synchronized void initMeasurementPositionDetectionList() {
        if (mMeasurementPositionDetectionList == null) {
            List<IntegerStringPair> list = Collections.synchronizedList(new ArrayList<>());
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_PROPER_MEASUREMENT_POSITION
                    , mApplicationContext.getString(R.string.proper_measurement_position)));
            list.add(new IntegerStringPair(BloodPressureMeasurementUtils.MEASUREMENT_STATUS_MEASUREMENT_POSITION_DETECTION_IMPROPER_MEASUREMENT_POSITION
                    , mApplicationContext.getString(R.string.improper_measurement_position)));
            mMeasurementPositionDetectionList = Collections.unmodifiableList(list);
        }
    }

    @NonNull
    public Flowable<List<DeviceSetting>> loadAllDeviceSetting() {
        return mDeviceSettingDataSource.loadAllDeviceSetting();
    }

    public Single<DeviceSetting> loadDeviceSettingById(long id) {
        return mDeviceSettingDataSource.loadDeviceSettingById(id);
    }

    @NonNull
    public Completable insertDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return mDeviceSettingDataSource.insertDeviceSetting(deviceSetting);
    }

    @NonNull
    public Completable deleteDeviceSetting(@NonNull DeviceSetting deviceSetting) {
        return mDeviceSettingDataSource.deleteDeviceSetting(deviceSetting);
    }

    @NonNull
    public Completable deleteAllDeviceSetting() {
        return mDeviceSettingDataSource.deleteAllDeviceSetting();
    }

    @Nullable
    public Integer getDeviceTypeImageResId(int deviceType) {
        initDataType();
        return mDeviceTypeImageResIdMap.get(deviceType);
    }

    @Nullable
    public String getDeviceTypeName(int deviceType) {
        initDataType();
        return mDeviceTypeNameMap.get(deviceType);
    }

    @NonNull
    public Map<Integer, Integer> provideDeviceTypeImageResMap() {
        initDataType();
        return mDeviceTypeImageResIdMap;
    }

    @NonNull
    public List<Pair<Integer, String>> provideDeviceTypeList() {
        initDataType();
        return mDeviceTypeNameList;
    }

    @NonNull
    public List<Pair<Integer, String>> provideDateTimeMonthList() {
        initDateTimeMonthList();
        return mDateTimeMonthList;
    }

    @NonNull
    public List<Pair<Integer, String>> provideDateTimeDayList() {
        initDateTimeDayList();
        return mDateTimeDayList;
    }

    @NonNull
    public List<String> provideDateTimeHoursList() {
        initDateTimeHoursList();
        return mDateTimeHoursList;
    }

    @NonNull
    public List<String> provideDateTimeMinutesList() {
        initDateTimeMinutesList();
        return mDateTimeMinutesList;
    }

    @NonNull
    public List<String> provideDateTimeSecondsList() {
        initDateTimeSecondsList();
        return mDateTimeSecondsList;
    }

    @NonNull
    public List<Pair<Integer, String>> provideBodyMovementDetectionList() {
        initBodyMovementDetectionList();
        return mBodyMovementDetectionList;
    }

    @NonNull
    public List<Pair<Integer, String>> provideCuffFitDetectionList() {
        initCuffFitDetectionList();
        return mCuffFitDetectionList;
    }

    @NonNull
    public List<Pair<Integer, String>> provideIrregularPulseDetectionList() {
        initIrregularPulseDetectionList();
        return mIrregularPulseDetectionList;
    }

    @NonNull
    public List<Pair<Integer, String>> providePulseRateRangeDetectionList() {
        initPulseRateRangeDetectionList();
        return mPulseRateRangeDetectionList;
    }

    @NonNull
    public List<Pair<Integer, String>> provideMeasurementPositionDetectionList() {
        initMeasurementPositionDetectionList();
        return mMeasurementPositionDetectionList;
    }

    @Nullable
    public String getDeviceSettingNameErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (TextUtils.isEmpty(charSequence)) {
            errorString = mApplicationContext.getString(R.string.no_value);
        }
        return errorString;
    }

    @Nullable
    public String getSystolicErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                new IEEE_11073_20601_SFLOAT(Double.parseDouble(charSequence.toString()));
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getDiastolicErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                new IEEE_11073_20601_SFLOAT(Double.parseDouble(charSequence.toString()));
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getMeanArterialPressureErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                new IEEE_11073_20601_SFLOAT(Double.parseDouble(charSequence.toString()));
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getPulseRateErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                new IEEE_11073_20601_SFLOAT(Double.parseDouble(charSequence.toString()));
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getUserIdErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                int userId = Integer.parseInt(charSequence.toString());
                if (userId < 0 || userId > 255) {
                    errorString = mApplicationContext.getString(R.string.out_of_range);
                }
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getNotificationCountErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                int notificationCount = Integer.parseInt(charSequence.toString());
                if (notificationCount < -1 || notificationCount == 0) {
                    errorString = mApplicationContext.getString(R.string.out_of_range);
                }
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getIndicationCountErrorString(@Nullable CharSequence charSequence) {
        return getNotificationCountErrorString(charSequence);
    }

    @Nullable
    public String getDateTimeYearErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                int year = Integer.parseInt(charSequence.toString());
                if ((year < 1582 || year > 9999) && year != DateTimeUtils.YEAR_IS_NOT_KNOWN) {
                    errorString = mApplicationContext.getString(R.string.out_of_range);
                }
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getResponseDelayErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                long responseDelay = Long.parseLong(charSequence.toString());
                if (responseDelay < 0) {
                    errorString = mApplicationContext.getString(R.string.out_of_range);
                }
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getResponseCodeErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                int errorCode = Integer.parseInt(charSequence.toString(), 16);
                if (errorCode < ErrorCode.INVALID_HANDLE || errorCode > ErrorCode.OUT_OF_RANGE) {
                    errorString = mApplicationContext.getString(R.string.out_of_range);
                }
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getCurrentCuffPressureErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                new IEEE_11073_20601_SFLOAT(Double.parseDouble(charSequence.toString()));
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getManufacturerNameStringErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (TextUtils.isEmpty(charSequence)) {
            errorString = mApplicationContext.getString(R.string.no_value);
        }
        return errorString;
    }

    @Nullable
    public String getModelNumberStringErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (TextUtils.isEmpty(charSequence)) {
            errorString = mApplicationContext.getString(R.string.no_value);
        }
        return errorString;
    }

    @Nullable
    public String getManufacturerIdentifierErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                long value = Long.parseLong(charSequence.toString());
                if (0 > value || 1099511627775L < value) {
                    errorString = mApplicationContext.getString(R.string.out_of_range);
                }
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @Nullable
    public String getOrganizationallyUniqueIdentifierErrorString(@Nullable CharSequence charSequence) {
        String errorString = null;

        if (charSequence == null || charSequence.length() == 0) {
            errorString = mApplicationContext.getString(R.string.no_value);
        } else {
            try {
                int value = Integer.parseInt(charSequence.toString());
                if (0 > value || 16777215 < value) {
                    errorString = mApplicationContext.getString(R.string.out_of_range);
                }
            } catch (NumberFormatException e) {
                e.printStackTrace();
                errorString = mApplicationContext.getString(R.string.wrong_format);
            }
        }
        return errorString;
    }

    @NonNull
    public String getUnitString(boolean isMmhg) {
        String result;
        if (isMmhg) {
            result = mApplicationContext.getString(R.string.mmhg);
        } else {
            result = mApplicationContext.getString(R.string.kpa);
        }
        return result;
    }

    @NonNull
    public String getHexString(int target, int length) {
        return String.format(Locale.US
                , "0x%1$0" + length + "x", target);
    }

    @NonNull
    public String getDateTimeString(int year, int month, int day, int hours, int minutes, int seconds) {
        return String.format(Locale.US
                , "%1$04d-%2$02d-%3$02d %4$02d:%5$02d:%6$02d"
                , year
                , month
                , day
                , hours
                , minutes
                , seconds);
    }

    @NonNull
    public String getIndicationsDisabledString() {
        return mApplicationContext.getString(R.string.indication_disabled);
    }

    @NonNull
    public String getIndicationsEnabledString() {
        return mApplicationContext.getString(R.string.indication_enabled);
    }

    @NonNull
    public String getNotificationsDisabledString() {
        return mApplicationContext.getString(R.string.notification_disabled);
    }

    @NonNull
    public String getNotificationsEnabledString() {
        return mApplicationContext.getString(R.string.notification_enabled);
    }

    @NonNull
    public String getIndicationsString(boolean enabled) {
        String result;
        if (enabled) {
            result = getIndicationsEnabledString();
        } else {
            result = getIndicationsDisabledString();
        }
        return result;
    }

    @NonNull
    public String getNotificationsString(boolean enabled) {
        String result;
        if (enabled) {
            result = getNotificationsEnabledString();
        } else {
            result = getNotificationsDisabledString();
        }
        return result;
    }

}
