package org.im97mori.ble.android.peripheral.persistence;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

@Entity(tableName = "devices")
public class Device {

    @PrimaryKey(autoGenerate = true)
    @ColumnInfo(name = "deviceid")
    private final long mId;

    @NonNull
    @ColumnInfo(name = "devicename")
    private String mDeviceName;

    @ColumnInfo(name = "devicetype")
    private final int mDeviceType;

    @ColumnInfo(name = "devicesetting")
    private String mDeviceSetting;

    @Ignore
    public Device(@NonNull String deviceName, int deviceType) {
        mId = 0;
        mDeviceName = deviceName;
        mDeviceType = deviceType;
    }

    public Device(long id, @NonNull String deviceName, int deviceType, @Nullable String deviceSetting) {
        mId = id;
        mDeviceName = deviceName;
        mDeviceType = deviceType;
        mDeviceSetting = deviceSetting;
    }

    public long getId() {
        return mId;
    }

    @NonNull
    public String getDeviceName() {
        return mDeviceName;
    }


    public void setDeviceName(@NonNull String deviceName) {
        this.mDeviceName = deviceName;
    }

    public int getDeviceType() {
        return mDeviceType;
    }

    @Nullable
    public String getDeviceSetting() {
        return mDeviceSetting;
    }

    public void setDeviceSetting(@Nullable String deviceSetting) {
        this.mDeviceSetting = deviceSetting;
    }

}
