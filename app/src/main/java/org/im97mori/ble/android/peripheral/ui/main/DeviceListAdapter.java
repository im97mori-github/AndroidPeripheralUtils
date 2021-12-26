package org.im97mori.ble.android.peripheral.ui.main;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class DeviceListAdapter extends ArrayAdapter<DeviceSetting> {

    private final LayoutInflater mLayoutInflater;

    private final Map<Integer, Integer> mDeviceTypeImageResMap;

    private final List<DeviceSetting> mDeviceSettingList;

    public DeviceListAdapter(@NonNull Context context
            , @NonNull Map<Integer, Integer> deviceTypeImageResMap
            , @NonNull List<DeviceSetting> deviceSettingList) {
        super(context, 0, new ArrayList<>(deviceSettingList));
        mDeviceTypeImageResMap = deviceTypeImageResMap;
        mDeviceSettingList = new ArrayList<>(deviceSettingList);
        mLayoutInflater = LayoutInflater.from(context);
    }

    public void setDeviceList(@NonNull List<DeviceSetting> deviceSettingList) {
        mDeviceSettingList.clear();
        mDeviceSettingList.addAll(deviceSettingList);
        clear();
        addAll(deviceSettingList);
    }

    @Override
    public long getItemId(int position) {
        return mDeviceSettingList.get(position).getId();
    }

    @NonNull
    @Override
    public View getView(int position, @Nullable View convertView, @NonNull ViewGroup parent) {
        DeviceSetting deviceSetting = getItem(position);
        if (convertView == null) {
            convertView = mLayoutInflater.inflate(R.layout.main_grid, parent, false);
        }
        TextView textView = convertView.findViewById(R.id.grid_text);
        textView.setText(deviceSetting.getDeviceSettingName());
        textView.setCompoundDrawablesRelativeWithIntrinsicBounds(0
                , Objects.requireNonNull(mDeviceTypeImageResMap.get(deviceSetting.getDeviceType()))
                , 0
                , 0);
        return convertView;
    }

}
